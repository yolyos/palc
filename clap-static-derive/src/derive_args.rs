use darling::FromDeriveInput;
use darling::util::Override;
use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, format_ident, quote, quote_spanned};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{DeriveInput, Generics, Ident, Token, Visibility};

use crate::common::{
    Arg, ArgField, ArgOrCommand, ArgTyKind, NamedArg, TY_OPTION, strip_ty_ctor, wrap_anon_item,
};

#[derive(FromDeriveInput)]
#[darling(supports(struct_named))]
pub struct ArgsItemDef {
    pub vis: Visibility,
    pub ident: Ident,
    pub generics: Generics,
    pub data: darling::ast::Data<(), ArgField>,
}

pub fn expand(input: DeriveInput, is_parser: bool) -> TokenStream {
    let mut tts = match ArgsItemDef::from_derive_input(&input) {
        Err(err) => err.write_errors(),
        Ok(def) => match expand_args_impl(def, is_parser) {
            Ok(tts) => return wrap_anon_item(tts),
            Err(err) => err.into_compile_error(),
        },
    };

    // Error fallback impl.
    let name = &input.ident;
    tts.extend(wrap_anon_item(quote! {
        #[automatically_derived]
        impl __rt::Parser for #name {}

        #[automatically_derived]
        impl __rt::Args for #name {}

        #[automatically_derived]
        impl __rt::ArgsInternal for #name {
            type __State = __rt::FallbackState<#name>;
        }
    }));
    tts
}

/// For `derive({Args,Parser})`.
pub struct ArgsImpl {
    is_parser: bool,
    state: ParserStateDefImpl,
}

fn expand_args_impl(def: ArgsItemDef, is_parser: bool) -> syn::Result<ArgsImpl> {
    let fields = def.data.take_struct().expect("checked by darling").fields;

    if !def.generics.params.is_empty() || def.generics.where_clause.is_some() {
        return Err(syn::Error::new(
            def.ident.span(),
            "TODO: generics are not supported yet",
        ));
    }

    let state_name = format_ident!("{}State", def.ident);
    let struct_name = def.ident.to_token_stream();
    let state = expand_state_def_impl(def.vis, state_name, struct_name, &fields)?;
    Ok(ArgsImpl { is_parser, state })
}

impl ToTokens for ArgsImpl {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { is_parser, state } = self;
        let struct_name = &state.output_ty;
        let state_name = &state.state_name;

        if *is_parser {
            tokens.extend(quote! {
                #[automatically_derived]
                impl __rt::Parser for #struct_name {}
            });
        }

        tokens.extend(quote! {
            #[automatically_derived]
            impl __rt::Args for #struct_name {}

            #[automatically_derived]
            impl __rt::ArgsInternal for #struct_name {
                type __State = #state_name;
            }

            #state
        });
    }
}

pub struct ParserStateDefImpl {
    pub vis: Visibility,
    pub state_name: Ident,
    pub output_ty: TokenStream,
    pub output_ctor: Option<TokenStream>,
    fields: Vec<FieldInfo>,

    match_named_arms: TokenStream,
    flatten_fields: Vec<usize>,
    handle_unnamed: TokenStream,
    handle_last_unnamed: Option<TokenStream>,
}

struct FieldInfo {
    name: Ident,
    state_ty: TokenStream,
    default: TokenStream,
    display_name: String,
    finish: FieldFinish,
}

enum FieldFinish {
    Id,
    Unwrap,
    UnwrapSubcommand,
    Finish,
}

pub fn expand_state_def_impl(
    vis: Visibility,
    state_name: Ident,
    output_ty: TokenStream,
    input_fields: &[ArgField],
) -> syn::Result<ParserStateDefImpl> {
    let mut out = ParserStateDefImpl {
        vis,
        state_name,
        output_ty,
        output_ctor: None,
        fields: Vec::with_capacity(input_fields.len()),
        match_named_arms: TokenStream::new(),
        flatten_fields: Vec::new(),
        handle_unnamed: TokenStream::new(),
        handle_last_unnamed: None,
    };
    let mut variable_arg_span = None;
    let mut check_variable_arg = |span: Span| -> syn::Result<()> {
        if let Some(prev_span) = variable_arg_span.replace(span) {
            let mut err = syn::Error::new(
                span,
                "there can only be one variable length argument or subcommand",
            );
            err.combine(syn::Error::new(prev_span, "previous argument here"));
            return Err(err);
        }
        Ok(())
    };

    for field in input_fields {
        let field_name = field.ident.clone().expect("checked by darling");
        let field_name_str = field_name.to_string();
        let field_ty = &field.ty;

        match &field.attrs {
            ArgOrCommand::Arg(arg) => {
                let arg_kind = field.arg_ty_kind();
                let (value_ty, state_ty, default, finish) = match arg_kind {
                    ArgTyKind::Bool => {
                        if matches!(arg, Arg::Unnamed) {
                            return Err(syn::Error::new(
                                field_name.span(),
                                "positional arguments cannot be `bool`",
                            ));
                        }
                        (
                            quote! { bool },
                            quote! { bool },
                            quote! { false },
                            FieldFinish::Id,
                        )
                    }
                    ArgTyKind::Vec(subty) => (
                        quote! { #subty },
                        quote! { #field_ty },
                        quote! { __rt::Vec::new() },
                        FieldFinish::Id,
                    ),
                    ArgTyKind::OptionVec(subty) | ArgTyKind::Option(subty) => (
                        quote! { #subty },
                        quote! { #field_ty },
                        quote! { __rt::None },
                        FieldFinish::Id,
                    ),
                    ArgTyKind::Convert => (
                        quote! { #field_ty },
                        quote! { __rt::Option<#field_ty> },
                        quote! { __rt::None },
                        FieldFinish::Unwrap,
                    ),
                };
                let parser =
                    quote_spanned!(value_ty.span()=> (__rt::arg_value_info!(#value_ty).parser));

                let mut display_name = String::new();
                match arg {
                    Arg::Unnamed => {
                        display_name = heck::AsShoutySnakeCase(field_name_str).to_string();
                        match arg_kind {
                            ArgTyKind::Bool => unreachable!(),
                            ArgTyKind::Option(_) | ArgTyKind::Convert => {
                                out.handle_unnamed.extend(quote! {
                                    if self.#field_name.is_none() {
                                        self.#field_name = __rt::Some(#parser(__rt::take_arg(__arg))?);
                                        return __rt::Ok(__rt::None);
                                    }
                                });
                            }
                            ArgTyKind::Vec(_) => {
                                check_variable_arg(field_name.span())?;
                                out.handle_last_unnamed = Some(quote! {{
                                    self.#field_name.push(#parser(__rt::take_arg(__arg))?);
                                    __rt::Ok(__rt::None)
                                }});
                            }
                            ArgTyKind::OptionVec(_) => {
                                check_variable_arg(field_name.span())?;
                                out.handle_last_unnamed = Some(quote! {{
                                    self.#field_name.get_or_insert_default().push(#parser(__rt::take_arg(__arg))?);
                                    __rt::Ok(__rt::None)
                                }});
                            }
                        }
                    }
                    Arg::Named(NamedArg { long, short }) => {
                        let mut pats = <Punctuated<String, Token![|]>>::new();

                        if let Some(ch) = short {
                            let ch = ch.clone().unwrap_or_else(|| {
                                field_name_str.chars().next().expect("must have name")
                            });
                            display_name = format!("-{ch}");
                            pats.push(ch.to_string());
                        }
                        if let Some(name) = long {
                            let long_name = match name {
                                Override::Inherit => {
                                    format!("--{}", heck::AsKebabCase(&field_name_str))
                                }
                                Override::Explicit(s) => format!("--{s}"),
                            };
                            // Prefer long names for display.
                            display_name = long_name.clone();
                            pats.push(long_name);
                        }
                        assert!(!pats.is_empty(), "validated by darling");

                        let handler = match arg_kind {
                            ArgTyKind::Bool => quote! {{
                                self.#field_name = true;
                                __rt::place_for_flag()
                            }},
                            ArgTyKind::Vec(_) => quote! {
                                __rt::place_for_vec(&mut self.#field_name, #parser)
                            },
                            ArgTyKind::OptionVec(_) => quote! {
                                __rt::place_for_vec(self.#field_name.get_or_insert_default(), #parser)
                            },
                            ArgTyKind::Option(_) | ArgTyKind::Convert => quote! {
                                __rt::place_for_set_value(&mut self.#field_name, #parser)
                            },
                        };

                        out.match_named_arms.extend(quote! { #pats => #handler, });
                    }
                }

                out.fields.push(FieldInfo {
                    name: field_name,
                    default,
                    state_ty,
                    display_name,
                    finish,
                });
            }
            ArgOrCommand::Subcommand => {
                check_variable_arg(field_name.span())?;

                let (subcmd_ty, finish) = match strip_ty_ctor(field_ty, TY_OPTION) {
                    Some(arg_ty) => (quote! { #arg_ty }, FieldFinish::Id),
                    None => (quote! { #field_ty }, FieldFinish::UnwrapSubcommand),
                };

                out.handle_last_unnamed = Some(quote! {
                    __rt::place_for_subcommand(&mut self.#field_name)
                });
                out.fields.push(FieldInfo {
                    name: field_name,
                    state_ty: quote! { __rt::Option<#subcmd_ty> },
                    default: quote! { __rt::None },
                    display_name: "SUBCOMMAND".into(),
                    finish,
                });
            }
            ArgOrCommand::Flatten => {
                out.flatten_fields.push(out.fields.len());
                // FIXME: This is lengthy and slow.
                out.handle_unnamed.extend(quote! {
                    match __rt::ParserStateDyn::feed_unnamed(&mut self.#field_name, __arg) {
                        __rt::Err(__rt::None) => {},
                        __ret => return __ret
                    }
                });
                out.fields.push(FieldInfo {
                    name: field_name,
                    state_ty: quote_spanned!(field_ty.span()=> <#field_ty as __rt::ArgsInternal>::__State),
                    default: quote_spanned!(field_ty.span()=> __rt::ParserState::init()),
                    display_name: String::new(),
                    finish: FieldFinish::Finish,
                });
            }
        }
    }

    Ok(out)
}

impl ToTokens for ParserStateDefImpl {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            vis,
            state_name,
            output_ty,
            output_ctor,
            fields,
            match_named_arms,
            flatten_fields,
            handle_unnamed,
            handle_last_unnamed,
        } = self;
        let output_ctor = output_ctor.as_ref().unwrap_or(&self.output_ty);

        let check_missing = fields
            .iter()
            .filter_map(|f| {
                let name = &f.name;
                let display_name = &f.display_name;
                match f.finish {
                    FieldFinish::Id | FieldFinish::Finish => None,
                    FieldFinish::Unwrap => Some(quote! {
                        if self.#name.is_none() {
                            return __rt::missing_required_arg(#display_name);
                        }
                    }),
                    FieldFinish::UnwrapSubcommand => Some(quote! {
                        if self.#name.is_none() {
                            return __rt::missing_required_subcmd();
                        }
                    }),
                }
            })
            .collect::<TokenStream>();

        let field_names = fields.iter().map(|f| &f.name).collect::<Vec<_>>();
        let field_tys = fields.iter().map(|f| &f.state_ty);
        let field_defaults = fields.iter().map(|f| &f.default);
        let field_finishes = fields.iter().map(|f| {
            let name = &f.name;
            match f.finish {
                FieldFinish::Id => quote! { self.#name },
                FieldFinish::Unwrap | FieldFinish::UnwrapSubcommand => {
                    quote! { self.#name.unwrap() }
                }
                FieldFinish::Finish => {
                    quote! { __rt::ParserState::finish(self.#name)? }
                }
            }
        });

        let handle_last_unnamed = handle_last_unnamed.clone().unwrap_or_else(|| {
            quote! { __rt::Err(__rt::None) }
        });

        let match_named_last = match &**flatten_fields {
            [] => quote!(return __rt::None),
            [prevs @ .., last] => {
                let names = prevs.iter().map(|&idx| &fields[idx].name);
                let last_name = &fields[*last].name;
                quote! {
                    #(if let __rt::Some(__r) = __rt::ParserStateDyn::feed_named(&mut self.#names, __name) {
                        __r
                    } else)* {
                        return __rt::ParserStateDyn::feed_named(&mut self.#last_name, __name)
                    }
                }
            }
        };

        tokens.extend(quote! {
            // Inherited visibility is needed to avoid "private type in public interface".
            // It is always invisible from user site because we are in an anonymous scope.
            #vis struct #state_name {
                #(#field_names : #field_tys,)*
            }

            #[automatically_derived]
            impl __rt::ParserState for #state_name {
                type Output = #output_ty;

                fn init() -> Self {
                    Self {
                        #(#field_names : #field_defaults,)*
                    }
                }

                fn finish(self) -> __rt::Result<Self::Output> {
                    #check_missing
                    __rt::Ok(#output_ctor {
                        #(#field_names : #field_finishes,)*
                    })
                }
            }

            #[automatically_derived]
            impl __rt::ParserStateDyn for #state_name {
                // TODO: Simplify for zero arms.
                #[allow(unreachable_code)]
                fn feed_named(&mut self, __name: &__rt::str) -> __rt::FeedNamed<'_> {
                    __rt::Some(match __name {
                        #match_named_arms
                        _ => #match_named_last
                    })
                }

                fn feed_unnamed(&mut self, __arg: &mut __rt::OsString) -> __rt::FeedUnnamed {
                    #handle_unnamed
                    #handle_last_unnamed
                }
            }
        });
    }
}
