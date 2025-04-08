use darling::FromDeriveInput;
use darling::util::Override;
use proc_macro2::TokenStream;
use quote::{ToTokens, format_ident, quote, quote_spanned};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{DeriveInput, Error, Generics, Ident, Token, Visibility};

use crate::common::{
    Arg, ArgField, ArgOrCommand, ArgTyKind, TY_OPTION, strip_ty_ctor, wrap_anon_item,
};

#[derive(FromDeriveInput)]
#[darling(supports(struct_named))]
pub struct ParserItemDef {
    pub vis: Visibility,
    pub ident: Ident,
    pub generics: Generics,
    pub data: darling::ast::Data<(), ArgField>,
}

pub fn expand(input: DeriveInput) -> TokenStream {
    match ParserItemDef::from_derive_input(&input) {
        Err(err) => err.write_errors(),
        Ok(def) => match expand_parser_impl(def) {
            Ok(tts) => wrap_anon_item(tts),
            Err(err) => err.into_compile_error(),
        },
    }
}

pub struct ParserImpl {
    state: ParserStateDefImpl,
}

pub fn expand_parser_impl(def: ParserItemDef) -> syn::Result<ParserImpl> {
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
    Ok(ParserImpl { state })
}

impl ToTokens for ParserImpl {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let state = &self.state;
        let struct_name = &state.output_ty;
        let state_name = &state.state_name;

        tokens.extend(quote! {
            #[automatically_derived]
            impl __rt::Parser for #struct_name {}

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
    handle_unnamed: TokenStream,
    handle_last_unnamed: Option<TokenStream>,
    subcommand: Option<SubcommandInfo>,
}

struct FieldInfo {
    name: Ident,
    state_ty: TokenStream,
    display_name: String,
    needs_unwrap: bool,
}

struct SubcommandInfo {
    field: Ident,
    ty: TokenStream,
    needs_unwrap: bool,
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
        handle_unnamed: TokenStream::new(),
        handle_last_unnamed: None,
        subcommand: None,
    };

    for field in input_fields {
        let field_name = field.ident.clone().expect("checked by darling");
        let field_name_str = field_name.to_string();
        let field_ty = &field.ty;

        let arg_kind = field.arg_ty_kind();
        let (state_ty, parser, needs_unwrap) = match arg_kind {
            ArgTyKind::Bool => {
                if let ArgOrCommand::None = &field.attrs {
                    return Err(syn::Error::new(
                        field_name.span(),
                        "positional arguments cannot be `bool`",
                    ));
                }
                (quote! { bool }, TokenStream::new(), false)
            }
            ArgTyKind::Vec(subty) => (
                quote! { #field_ty },
                quote_spanned!(subty.span()=> (__rt::arg_value_info!(#subty).parser)),
                false,
            ),
            ArgTyKind::Option(subty) => (
                quote! { #field_ty },
                quote_spanned!(subty.span()=> (__rt::arg_value_info!(#subty).parser)),
                false,
            ),
            ArgTyKind::Convert => (
                quote! { __rt::Option<#field_ty> },
                quote_spanned!(field_ty.span()=> (__rt::arg_value_info!(#field_ty).parser)),
                true,
            ),
        };

        let mut display_name = String::new();

        match &field.attrs {
            // Unnamed (positional) arguments.
            ArgOrCommand::None => {
                if out.handle_last_unnamed.is_some() {
                    return Err(syn::Error::new(
                        field_name.span(),
                        "additional positional arguments are not allowed \
                        after a catch-all argument",
                    ));
                }

                display_name = heck::AsShoutySnakeCase(field_name_str).to_string();
                match arg_kind {
                    ArgTyKind::Bool => unreachable!(),
                    ArgTyKind::Option(_) | ArgTyKind::Convert => {
                        out.handle_unnamed.extend(quote! {
                            if self.#field_name.is_none() {
                                self.#field_name = __rt::Some(#parser(__rt::take_arg(__arg))?);
                                return __rt::Ok(());
                            }
                        });
                    }
                    ArgTyKind::Vec(_) => {
                        out.handle_last_unnamed = Some(quote! {{
                            self.#field_name.push(#parser(__rt::take_arg(__arg))?);
                            __rt::Ok(())
                        }});
                    }
                }
            }
            // Named arguments.
            ArgOrCommand::Arg(Arg { long, short }) => {
                let mut pats = <Punctuated<String, Token![|]>>::new();

                if let Some(ch) = short {
                    let ch = ch
                        .clone()
                        .unwrap_or_else(|| field_name_str.chars().next().expect("must have name"));
                    display_name = format!("-{ch}");
                    pats.push(ch.to_string());
                }
                if let Some(name) = long {
                    let long_name = match name {
                        Override::Inherit => format!("--{}", heck::AsKebabCase(&field_name_str)),
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
                        __rt::Ok(__rt::FeedNamedOk::Arg0)
                    }},
                    ArgTyKind::Vec(_) => quote! {
                        __rt::Ok(__rt::FeedNamedOk::Arg1(|__s, __v| {
                            __s.#field_name.push(#parser(__v)?);
                            __rt::Ok(())
                        }))
                    },
                    ArgTyKind::Option(_) | ArgTyKind::Convert => quote! {
                        __rt::Ok(__rt::FeedNamedOk::Arg1(|__s, __v| {
                            __s.#field_name = Some(#parser(__v)?);
                            __rt::Ok(())
                        }))
                    },
                };

                out.match_named_arms.extend(quote! { #pats => #handler, });
            }
            // Sub-struct injection.
            ArgOrCommand::Command(cmd) => {
                assert!(cmd.subcommand, "validated");
                if out.subcommand.is_some() {
                    return Err(Error::new(
                        field_name.span(),
                        "duplicated `#[command(subcommand)]`",
                    ));
                }
                out.subcommand = Some(if let Some(arg_ty) = strip_ty_ctor(field_ty, TY_OPTION) {
                    SubcommandInfo {
                        field: field_name.clone(),
                        ty: quote! { #arg_ty },
                        needs_unwrap: false,
                    }
                } else {
                    SubcommandInfo {
                        field: field_name.clone(),
                        ty: quote! { #field_ty },
                        needs_unwrap: true,
                    }
                });
                continue;
            }
        }

        out.fields.push(FieldInfo {
            name: field_name,
            state_ty,
            display_name,
            needs_unwrap,
        });
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
            handle_unnamed,
            handle_last_unnamed,
            subcommand,
        } = self;
        let output_ctor = output_ctor.as_ref().unwrap_or(&self.output_ty);

        let mut check_missing = fields
            .iter()
            .filter_map(|f| {
                let name = &f.name;
                let display_name = &f.display_name;
                f.needs_unwrap.then(|| {
                    quote! {
                        if self.#name.is_none() {
                            return __rt::missing_required_arg(#display_name);
                        }
                    }
                })
            })
            .collect::<TokenStream>();

        let field_names = fields.iter().map(|f| &f.name).collect::<Vec<_>>();
        let field_tys = fields.iter().map(|f| &f.state_ty);
        let field_finishes = fields.iter().map(|f| {
            let name = &f.name;
            if f.needs_unwrap {
                quote! { self.#name.unwrap() }
            } else {
                quote! { self.#name }
            }
        });

        let handle_last_unnamed = handle_last_unnamed.clone().unwrap_or_else(|| {
            quote! { __rt::Err(__rt::None) }
        });

        let (subcommand_ty, subcommand_finish) = match subcommand {
            None => (quote! { __rt::Infallible }, TokenStream::new()),
            Some(SubcommandInfo {
                field,
                ty,
                needs_unwrap,
            }) => {
                if *needs_unwrap {
                    check_missing.extend(quote! {
                        if __subcmd.is_none() {
                            return __rt::missing_required_subcmd();
                        }
                    });
                    (ty.clone(), quote! { #field : __subcmd.unwrap() })
                } else {
                    (ty.clone(), quote! { #field : __subcmd })
                }
            }
        };

        tokens.extend(quote! {
            // Inherited visibility is needed to avoid "private type in public interface".
            // It is always invisible from user site because we are in an anonymous scope.
            #[derive(Default)]
            #vis struct #state_name {
                #(#field_names : #field_tys,)*
            }

            #[automatically_derived]
            impl __rt::ParserState for #state_name {
                type Output = #output_ty;
                type Subcommand = #subcommand_ty;

                fn feed_named(&mut self, __name: &__rt::str) -> __rt::FeedNamed<Self> {
                    match __name {
                        #match_named_arms
                        _ => __rt::Err(__rt::None)
                    }
                }

                fn feed_unnamed(&mut self, __arg: &mut __rt::OsString) -> __rt::FeedUnnamed {
                    #handle_unnamed
                    #handle_last_unnamed
                }

                fn finish(self, __subcmd: __rt::Option<Self::Subcommand>) -> __rt::Result<Self::Output> {
                    #check_missing
                    __rt::Ok(#output_ctor {
                        #(#field_names : #field_finishes,)*
                        #subcommand_finish
                    })
                }
            }
        });
    }
}
