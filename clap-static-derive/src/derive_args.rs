use darling::FromDeriveInput;
use darling::util::Override;
use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, format_ident, quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{DeriveInput, Generics, Ident, Visibility};

use crate::common::{ArgField, ArgOrCommand, ArgTyKind, TY_OPTION, strip_ty_ctor, wrap_anon_item};

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
        Ok(def) => match expand_args_impl(&def, is_parser) {
            Ok(out) => return wrap_anon_item(out),
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
pub struct ArgsImpl<'i> {
    is_parser: bool,
    state: ParserStateDefImpl<'i>,
}

fn expand_args_impl(def: &ArgsItemDef, is_parser: bool) -> syn::Result<ArgsImpl<'_>> {
    let darling::ast::Data::Struct(fields) = &def.data else {
        unreachable!("checked by darling")
    };

    if !def.generics.params.is_empty() || def.generics.where_clause.is_some() {
        return Err(syn::Error::new(
            def.ident.span(),
            "TODO: generics are not supported yet",
        ));
    }

    let state_name = format_ident!("{}State", def.ident);
    let struct_name = def.ident.to_token_stream();
    let state = expand_state_def_impl(&def.vis, state_name, struct_name, &fields.fields)?;
    Ok(ArgsImpl { is_parser, state })
}

impl ToTokens for ArgsImpl<'_> {
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

pub struct ParserStateDefImpl<'i> {
    pub vis: &'i Visibility,
    pub state_name: Ident,
    pub output_ty: TokenStream,
    pub output_ctor: Option<TokenStream>,
    fields: Vec<FieldInfo<'i>>,
}

struct FieldInfo<'i> {
    name: &'i Ident,
    kind: FieldKind,
    /// The type used for parser inference, with `Option`/`Vec` stripped.
    effective_ty: &'i syn::Type,
    /// The original type specified by the user.
    original_ty: &'i syn::Type,
    /// Matching names for named arguments. Empty for unnamed arguments.
    arg_names: Vec<String>,
    /// Display name of the value.
    value_display: String,
    require_eq: bool,
    is_last: bool,
}

impl FieldInfo<'_> {
    /// The name used for error reporting.
    fn display_name(&self) -> &str {
        self.arg_names
            .get(0)
            .map(String::as_str)
            .unwrap_or(&self.value_display)
    }

    fn value_info(&self) -> TokenStream {
        let ty = self.effective_ty;
        quote_spanned! {ty.span()=> __rt::arg_value_info!(#ty) }
    }
}

enum FieldKind {
    BoolSetTrue,
    Option,
    UnwrapOption,
    Vec,
    OptionVec,
    OptionSubcommand,
    UnwrapSubcommand,
    Flatten,
}

impl FieldKind {
    pub fn is_trailing(&self) -> bool {
        matches!(
            self,
            Self::Vec | Self::OptionVec | Self::OptionSubcommand | Self::UnwrapSubcommand
        )
    }
}

pub fn expand_state_def_impl<'i>(
    vis: &'i Visibility,
    state_name: Ident,
    output_ty: TokenStream,
    input_fields: &'i [ArgField],
) -> syn::Result<ParserStateDefImpl<'i>> {
    let mut out = ParserStateDefImpl {
        vis,
        state_name,
        output_ty,
        output_ctor: None,
        fields: Vec::with_capacity(input_fields.len()),
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
        let field_name = field.ident.as_ref().expect("checked by darling");

        match &field.attrs {
            ArgOrCommand::Arg(arg) => {
                let arg_kind = field.arg_ty_kind();
                if arg.last
                    && (arg.is_named()
                        || !matches!(arg_kind, ArgTyKind::Vec(_) | ArgTyKind::OptionVec(_)))
                {
                    return Err(syn::Error::new(
                        field.ty.span(),
                        "arg(last) must be used on a `Vec`-like positional argument",
                    ));
                }

                let (kind, effective_ty) = match arg_kind {
                    ArgTyKind::Bool => (FieldKind::BoolSetTrue, &field.ty),
                    ArgTyKind::Vec(subty) => (FieldKind::Vec, subty),
                    ArgTyKind::OptionVec(subty) => (FieldKind::OptionVec, subty),
                    ArgTyKind::Option(subty) => (FieldKind::Option, subty),
                    ArgTyKind::Convert => (FieldKind::UnwrapOption, &field.ty),
                };

                let field_name_str = field_name.to_string();
                let mut arg_names = Vec::new();
                let mut value_display = String::new();
                if !arg.is_named() {
                    value_display = arg
                        .value_name
                        .clone()
                        .unwrap_or_else(|| heck::AsShoutySnakeCase(field_name_str).to_string());

                    if arg.trailing_var_arg
                        && !matches!(arg_kind, ArgTyKind::Vec(_) | ArgTyKind::OptionVec(_))
                    {
                        return Err(syn::Error::new(
                            field.ty.span(),
                            "arg(trailing_var_arg) can only be used on arguments with `Vec`-like types",
                        ));
                    }
                } else {
                    if let Some(ch) = &arg.short {
                        let ch = ch.clone().unwrap_or_else(|| {
                            field_name_str.chars().next().expect("must have name")
                        });
                        value_display = format!("-{ch}");
                        arg_names.push(ch.to_string());
                    }
                    for &short in arg.short_alias.iter() {
                        arg_names.push(short.to_string());
                    }
                    if let Some(name) = &arg.long {
                        let long_name = match name {
                            Override::Inherit => {
                                format!("--{}", heck::AsKebabCase(&field_name_str))
                            }
                            Override::Explicit(s) => format!("--{s}"),
                        };
                        // Prefer long names for display.
                        value_display = long_name.clone();
                        arg_names.push(long_name);
                    }
                    for long in arg.alias.iter() {
                        arg_names.push(format!("--{long}"));
                    }
                    assert!(!arg_names.is_empty());
                }

                out.fields.push(FieldInfo {
                    name: &field_name,
                    kind,
                    effective_ty,
                    original_ty: &field.ty,
                    arg_names,
                    value_display,
                    require_eq: arg.require_equals,
                    is_last: arg.last,
                });
            }
            ArgOrCommand::Subcommand => {
                check_variable_arg(field_name.span())?;
                let (kind, effective_ty) = match strip_ty_ctor(&field.ty, TY_OPTION) {
                    Some(subty) => (FieldKind::OptionSubcommand, subty),
                    None => (FieldKind::UnwrapSubcommand, &field.ty),
                };
                out.fields.push(FieldInfo {
                    name: &field_name,
                    kind,
                    effective_ty,
                    original_ty: &field.ty,
                    arg_names: Vec::new(),
                    value_display: "COMMAND".into(),
                    require_eq: false,
                    is_last: false,
                });
            }
            ArgOrCommand::Flatten => {
                out.fields.push(FieldInfo {
                    name: &field_name,
                    kind: FieldKind::Flatten,
                    effective_ty: &field.ty,
                    original_ty: &field.ty,
                    arg_names: Vec::new(),
                    value_display: String::new(),
                    require_eq: false,
                    is_last: false,
                });
            }
        }
    }

    Ok(out)
}

impl ToTokens for ParserStateDefImpl<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            vis,
            state_name,
            output_ty,
            output_ctor,
            fields,
        } = self;
        let output_ctor = output_ctor.as_ref().unwrap_or(&self.output_ty);

        // State field initialization and finalization.

        let check_missing = fields
            .iter()
            .filter_map(|f| {
                let name = f.name;
                let display_name = f.display_name();
                match f.kind {
                    FieldKind::BoolSetTrue
                    | FieldKind::Option
                    | FieldKind::Vec
                    | FieldKind::OptionVec
                    | FieldKind::OptionSubcommand
                    | FieldKind::Flatten => None,
                    FieldKind::UnwrapSubcommand => Some(quote! {
                        if self.#name.is_none() {
                            return __rt::missing_required_subcmd();
                        }
                    }),
                    FieldKind::UnwrapOption => Some(quote! {
                        if self.#name.is_none() {
                            return __rt::missing_required_arg(#display_name);
                        }
                    }),
                }
            })
            .collect::<TokenStream>();

        let field_names = fields.iter().map(|f| &f.name).collect::<Vec<_>>();
        let field_tys = fields
            .iter()
            .map(|f| {
                let effective_ty = &f.effective_ty;
                match f.kind {
                    FieldKind::BoolSetTrue
                    | FieldKind::Option
                    | FieldKind::Vec
                    | FieldKind::OptionVec
                    | FieldKind::OptionSubcommand => f.original_ty.to_token_stream(),
                    FieldKind::UnwrapOption | FieldKind::UnwrapSubcommand => {
                        quote! { __rt::Option<#effective_ty> }
                    }
                    FieldKind::Flatten => {
                        quote! { <#effective_ty as __rt::ArgsInternal>::__State }
                    }
                }
            })
            .collect::<Vec<_>>();
        let field_inits = fields.iter().map(|f| match f.kind {
            FieldKind::BoolSetTrue => quote! { false },
            FieldKind::Option
            | FieldKind::UnwrapOption
            | FieldKind::OptionVec
            | FieldKind::UnwrapSubcommand
            | FieldKind::OptionSubcommand => quote! { __rt::None },
            FieldKind::Vec => quote! { __rt::Vec::new() },
            FieldKind::Flatten => quote! { __rt::ParserState::init() },
        });
        let field_finishes = fields.iter().map(|f| {
            let name = f.name;
            match f.kind {
                FieldKind::BoolSetTrue
                | FieldKind::Option
                | FieldKind::Vec
                | FieldKind::OptionVec
                | FieldKind::OptionSubcommand => quote! { self.#name },
                FieldKind::UnwrapOption | FieldKind::UnwrapSubcommand => {
                    quote! { self.#name.unwrap() }
                }
                FieldKind::Flatten => quote! { __rt::ParserState::finish(self.#name)? },
            }
        });

        // Named argument dispatcher.

        let feed_named_arms = fields
            .iter()
            .filter(|f| !f.arg_names.is_empty())
            .map(|f| {
                let name = f.name;
                let arg_names = &f.arg_names;
                let require_eq = f.require_eq;
                let value_info = f.value_info();
                let span = f.effective_ty.span();
                let action = match f.kind {
                    FieldKind::BoolSetTrue => {
                        quote! {{ self.#name = true; __rt::place_for_flag() }}
                    }
                    FieldKind::Option | FieldKind::UnwrapOption => {
                        quote_spanned! {span=> __rt::place_for_set_value::<_, _, #require_eq>(&mut self.#name, #value_info) }
                    }
                    FieldKind::Vec => {
                        quote_spanned! {span=> __rt::place_for_vec::<_, _, #require_eq>(&mut self.#name, #value_info)  }
                    }
                    FieldKind::OptionVec => {
                        quote_spanned! {span=> __rt::place_for_vec::<_, _, #require_eq>(&mut self.#name, #value_info)  }
                    }
                    // Handle later.
                    FieldKind::Flatten => TokenStream::new(),
                    FieldKind::OptionSubcommand | FieldKind::UnwrapSubcommand => unreachable!(),
                };
                quote! {
                    #(#arg_names)|* => #action,
                }
            })
            .collect::<TokenStream>();
        let flatten_names = fields
            .iter()
            .filter(|f| matches!(f.kind, FieldKind::Flatten))
            .map(|f| f.name);
        let feed_named_else = quote! {
            #(if let __rt::Some(__r) = __rt::ParserStateDyn::feed_named(&mut self.#flatten_names, __name) {
                __rt::Some(__r)
            } else)*
        };
        let feed_named_func =
            (!feed_named_arms.is_empty() || !feed_named_else.is_empty()).then(|| {
                quote! {
                    fn feed_named(&mut self, __name: &__rt::str) -> __rt::FeedNamed<'_> {
                        __rt::Some(match __name {
                            #feed_named_arms
                            _ => return #feed_named_else { __rt::None }
                        })
                    }
                }
            });

        // Positional argument dispatcher.

        // FIXME: This is O(n) on each call.
        let mut feed_unnamed_body = fields.iter().filter_map(|f| {
            if !f.arg_names.is_empty() || f.is_last {
                return None;
            }
            let name = f.name;
            let value_info = f.value_info();
            let span = f.effective_ty.span();
            let parsed = quote_spanned! {span=>
                __rt::ArgValueInfo::parse(#value_info, __rt::take_arg(__arg))?
            };
            match f.kind {
                FieldKind::BoolSetTrue => unreachable!(),
                FieldKind::Option | FieldKind::UnwrapOption => Some(quote! {
                    if self.#name.is_none() {
                        self.#name = __rt::Some(#parsed);
                        return __rt::Ok(__rt::None);
                    }
                }),
                FieldKind::Flatten => Some(quote_spanned! {span=>
                    match __rt::ParserStateDyn::feed_unnamed(&mut self.#name, __arg, __is_last) {
                        __rt::Err(__rt::None) => {},
                        __ret => return __ret
                    }
                }),
                // Tail arguments.
                // FIXME: trailing_var_arg
                FieldKind::Vec => Some(quote! {
                    self.#name.push(#parsed);
                    __rt::Ok(__rt::None)
                }),
                FieldKind::OptionVec => Some(quote! {
                    self.#name.get_or_insert_default().push(#parsed);
                    __rt::Ok(__rt::None)
                }),
                FieldKind::OptionSubcommand |
                FieldKind::UnwrapSubcommand => Some(quote_spanned! {span=>
                    __rt::place_for_subcommand(&mut self.#name)
                }),
            }
        })
        .collect::<TokenStream>();
        let has_trailing_expr = fields.iter().any(|f| f.kind.is_trailing());
        let last_field = fields.iter().find(|f| f.is_last);

        let feed_unnamed_func = if feed_unnamed_body.is_empty() && last_field.is_none() {
            None
        } else {
            if !has_trailing_expr {
                feed_unnamed_body.extend(quote! {
                    __rt::Err(__rt::None)
                });
            }
            if let Some(f) = last_field {
                let name = f.name;
                let value_info = f.value_info();
                let get_or_insert = matches!(f.kind, FieldKind::OptionVec)
                    .then(|| quote! { .get_or_insert_default() });
                feed_unnamed_body = quote! {
                    if !__is_last {
                        #feed_unnamed_body
                    } else {
                        __rt::place_for_trailing_var_arg(&mut self.#name #get_or_insert, #value_info)
                    }
                };
            }
            Some(quote! {
                fn feed_unnamed(&mut self, __arg: &mut __rt::OsString, __is_last: bool) -> __rt::FeedUnnamed {
                    #feed_unnamed_body
                }
            })
        };

        tokens.extend(quote! {
            // FIXME: Visibility is still broken with `command(flatten)`.
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
                        #(#field_names : #field_inits,)*
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
                #feed_named_func
                #feed_unnamed_func
            }
        });
    }
}
