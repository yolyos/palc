use darling::FromDeriveInput;
use darling::util::Override;
use heck::ToKebabCase;
use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, format_ident, quote};
use syn::{DeriveInput, Error, Generics, Ident};

use crate::common::{
    Arg, ArgField, ArgOrCommand, ArgTyKind, TY_OPTION, strip_ty_ctor, wrap_anon_item,
};

#[derive(FromDeriveInput)]
#[darling(supports(struct_named))]
pub struct ParserItemDef {
    pub ident: Ident,
    pub generics: Generics,
    pub data: darling::ast::Data<(), ArgField>,
}

pub fn expand(input: DeriveInput) -> TokenStream {
    match ParserItemDef::from_derive_input(&input) {
        Err(err) => err.write_errors(),
        Ok(def) => match expand_impl(def) {
            Ok(tts) => wrap_anon_item(tts),
            Err(err) => err.into_compile_error(),
        },
    }
}

pub struct ParserImpl {
    pub builder: BuilderImpl,
}

pub struct BuilderImpl {
    pub struct_name: Ident,
    pub struct_ctor: TokenStream,
    pub builder_name: Ident,
    builder_fields: Vec<Ident>,
    builder_field_tys: Vec<TokenStream>,
    on_positional: TokenStream,
    act_defs: Vec<(Ident, TokenStream)>,
    long_args: Vec<(String, Ident)>,
    short_args: Vec<(char, Ident)>,
    finish_exprs: Vec<TokenStream>,
    subcommand_ty: TokenStream,
    subcommand_finish: Option<TokenStream>,
}

pub fn expand_impl(def: ParserItemDef) -> syn::Result<ParserImpl> {
    let fields = def.data.take_struct().expect("checked by darling").fields;

    if !def.generics.params.is_empty() || def.generics.where_clause.is_some() {
        return Err(syn::Error::new(
            def.ident.span(),
            "TODO: generics are not supported yet",
        ));
    }

    let struct_name = def.ident;
    let builder_name = format_ident!("Builder{struct_name}", span = Span::call_site());

    let mut builder_fields = Vec::new();
    let mut builder_field_tys = Vec::new();
    let mut act_defs = Vec::new();
    let mut finish_exprs = Vec::new();

    let mut short_args = Vec::new();
    let mut long_args = Vec::new();
    let mut subcommand_ty = quote!(rt::__internal::Infallible);
    let mut subcommand_finish = None;

    let mut on_positional = TokenStream::new();

    for field in &fields {
        let field_name = field.ident.as_ref().expect("checked by darling");
        let field_name_str = field_name.to_string();
        let field_name_lit = syn::LitStr::new(&field_name_str, field_name.span());
        let field_ty = &field.ty;

        let act_name = format_ident!(
            "__ACT_{struct_name}_FIELD_{field_name}",
            span = Span::call_site()
        );

        let is_positional = match &field.attrs {
            ArgOrCommand::None => true,
            ArgOrCommand::Arg(Arg { long, short }) => {
                if let Some(name) = long {
                    let name = match name {
                        Override::Inherit => field_name_str.to_kebab_case(),
                        Override::Explicit(s) => s.clone(),
                    };
                    long_args.push((name, act_name.clone()));
                }
                if let Some(ch) = short {
                    let ch = ch
                        .clone()
                        .unwrap_or_else(|| field_name_str.chars().next().expect("must have name"));
                    short_args.push((ch, act_name.clone()));
                }
                false
            }
            ArgOrCommand::Command(cmd) => {
                assert!(cmd.subcommand, "validated");
                if subcommand_finish.is_some() {
                    return Err(Error::new(
                        field_name.span(),
                        "duplicated `#[command(subcommand)]`",
                    ));
                }
                if let Some(arg_ty) = strip_ty_ctor(field_ty, TY_OPTION) {
                    subcommand_ty = quote! { #arg_ty };
                    subcommand_finish = Some(quote! { #field_name: __subcmd });
                } else {
                    subcommand_ty = quote! { #field_ty };
                    subcommand_finish =
                        Some(quote! { #field_name: rt::__internal::err_require_subcmd(__subcmd)? });
                }
                continue;
            }
        };

        let (builder_field_ty, parser, finish) = match field.arg_ty_kind() {
            ArgTyKind::Bool => {
                if is_positional {
                    return Err(syn::Error::new(
                        field_name.span(),
                        "positional arguments cannot be `bool`",
                    ));
                }
                (quote! { bool }, None, quote! { self.#field_name })
            }
            ArgTyKind::Option(subty) => (
                quote! { #field_ty },
                Some(quote! { <#subty as rt::FromValue>::try_from_value }),
                quote! { self.#field_name },
            ),
            ArgTyKind::Convert => (
                quote!(Option<#field_ty>),
                Some(quote! { <#field_ty as rt::FromValue>::try_from_value }),
                quote! { rt::__internal::err_require_arg(self.#field_name, #field_name_lit)? },
            ),
        };

        builder_fields.push(field_name.clone());
        builder_field_tys.push(builder_field_ty);
        finish_exprs.push(finish);

        if is_positional {
            let parser = parser.expect("checked above");
            on_positional.extend(quote! {
                if let rt::__internal::None = self.#field_name {
                    self.#field_name = Some(#parser(rt::__internal::Cow::Owned(__v))?);
                    return rt::__internal::Ok(());
                }
            });
        } else {
            let act = if let Some(parser) = parser {
                quote! {
                    KeyValue(|__b, __v| {
                        __b.#field_name = rt::__internal::Some(#parser(__v)?);
                        rt::__internal::Ok(())
                    })
                }
            } else {
                quote! { Flag(|__b| { __b.#field_name = true; rt::__internal::Ok(()) }) }
            };
            act_defs.push((act_name, act));
        }
    }

    short_args.sort_by_key(|tup| tup.0);
    long_args.sort_by(|lhs, rhs| Ord::cmp(&lhs.0, &rhs.0));

    Ok(ParserImpl {
        builder: BuilderImpl {
            struct_name,
            struct_ctor: quote! { Self::Output },
            builder_name,
            builder_fields,
            builder_field_tys,
            on_positional,
            act_defs,
            long_args,
            short_args,
            finish_exprs,
            subcommand_ty,
            subcommand_finish,
        },
    })
}

impl ToTokens for ParserImpl {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let builder = &self.builder;
        let struct_name = &builder.struct_name;
        let builder_name = &builder.builder_name;

        tokens.extend(quote! {
            #[automatically_derived]
            impl rt::Parser for #struct_name {}

            #[automatically_derived]
            impl rt::__internal::ParserInternal for #struct_name {
                type __Builder = #builder_name;
            }

            #builder
        });
    }
}

impl ToTokens for BuilderImpl {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            struct_name,
            struct_ctor,
            builder_name,
            builder_fields,
            builder_field_tys,
            finish_exprs,
            on_positional,
            subcommand_ty,
            subcommand_finish,
            ..
        } = self;

        let act_name = self.act_defs.iter().map(|(name, _)| name);
        let act_expr = self.act_defs.iter().map(|(_, expr)| expr);
        let short_ch = self.short_args.iter().map(|(ch, _)| *ch);
        let short_act = self.short_args.iter().map(|(_, act)| act);
        let long_str = self.long_args.iter().map(|(s, _)| s.as_str());
        let long_act = self.long_args.iter().map(|(_, act)| act);

        tokens.extend(quote! {
            #[derive(Default)]
            struct #builder_name {
                #(#builder_fields : #builder_field_tys,)*
            }

            #(const #act_name : rt::__internal::Action<#builder_name> =
                rt::__internal::Action:: #act_expr;)*

            #[automatically_derived]
            impl rt::__internal::ParserBuilder for #builder_name {
                type Output = #struct_name;
                type Subcommand = #subcommand_ty;

                const SHORT_ARGS: &'static [(rt::__internal::char, rt::__internal::Action<Self>)] =
                    &[#((#short_ch, #short_act)),*];
                const LONG_ARGS: &'static [(&'static rt::__internal::str, rt::__internal::Action<Self>)] =
                    &[#((#long_str, #long_act)),*];

                fn feed_positional(&mut self, __v: rt::__internal::OsString)
                    -> rt::__internal::Result<(), rt::Error>
                {
                    #on_positional
                    rt::__internal::err_extra_positional(__v)
                }

                fn finish(self, __subcmd: rt::__internal::Option<Self::Subcommand>)
                    -> rt::__internal::Result<Self::Output, rt::Error>
                {
                    rt::__internal::Ok(#struct_ctor {
                        #(#builder_fields : #finish_exprs,)*
                        #subcommand_finish
                    })
                }
            }
        });
    }
}
