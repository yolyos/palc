use darling::FromDeriveInput;
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote};
use syn::{DeriveInput, Generics, Ident};

use crate::common::{ArgField, ArgTyKind};

#[derive(FromDeriveInput)]
#[darling(supports(struct_named))]
struct ParserDef {
    ident: Ident,
    generics: Generics,
    data: darling::ast::Data<(), ArgField>,
}

pub(crate) fn expand(input: DeriveInput) -> TokenStream {
    match ParserDef::from_derive_input(&input) {
        Err(err) => err.write_errors(),
        Ok(def) => match expand_derive_parser_inner(def) {
            Ok(tts) => tts,
            Err(err) => err.into_compile_error(),
        },
    }
}

fn expand_derive_parser_inner(def: ParserDef) -> syn::Result<TokenStream> {
    let fields = def.data.take_struct().expect("checked by darling").fields;

    if !def.generics.params.is_empty() || def.generics.where_clause.is_some() {
        return Err(syn::Error::new(
            def.ident.span(),
            "TODO: generics are not supported yet",
        ));
    }

    let struct_name = &def.ident;
    let builder_name = format_ident!("Builder{}", def.ident, span = Span::call_site());
    let subcommand_ty = quote!(rt::__internal::Infallible);

    let mut builder_fields = TokenStream::new();
    let mut act_defs = TokenStream::new();
    let mut on_positional = TokenStream::new();
    let mut finish_fields = TokenStream::new();
    let mut short_args = Vec::new();
    let mut long_args = Vec::new();

    for field in &fields {
        let field_name = field.ident.as_ref().expect("checked by darling");
        let field_name_str = field_name.to_string();
        let field_name_lit = syn::LitStr::new(&field_name_str, field_name.span());
        let field_ty = &field.ty;
        let act_name = format_ident!("ACT_{}", field_name, span = Span::call_site());

        if let Some(name) = &field.long {
            let name = name.as_ref().unwrap_or(&field_name_str).clone();
            long_args.push((name, act_name.clone()));
        }
        if let Some(ch) = &field.short {
            let ch = ch
                .clone()
                .unwrap_or_else(|| field_name_str.chars().next().expect("must have name"));
            short_args.push((ch, act_name.clone()));
        }

        let is_positional = field.long.is_none() && field.short.is_none();

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
                quote! { rt::__internal::require_arg(self.#field_name, #field_name_lit) },
            ),
        };
        builder_fields.extend(quote! {
            #field_name : #builder_field_ty,
        });
        finish_fields.extend(quote! {
            #field_name : #finish,
        });

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
            act_defs.extend(quote! {
                #[allow(non_upper_case_globals)]
                const #act_name : rt::__internal::Action<#builder_name> =
                    rt::__internal::Action:: #act;
            });
        }
    }

    short_args.sort_unstable_by_key(|tup| tup.0);
    long_args.sort_unstable_by(|lhs, rhs| Ord::cmp(&lhs.0, &rhs.0));
    let short_args = short_args
        .into_iter()
        .flat_map(|(ch, ident)| quote! { (#ch, #ident), })
        .collect::<TokenStream>();
    let long_args = long_args
        .into_iter()
        .flat_map(|(s, ident)| quote! { (#s, #ident), })
        .collect::<TokenStream>();

    Ok(quote! {
        const _: () = {
            extern crate clap_static as rt;

            #[automatically_derived]
            impl rt::Parser for #struct_name {}

            #[automatically_derived]
            impl rt::__internal::ParserInternal for #struct_name {
                type __Builder = #builder_name;
            }

            #[derive(Default)]
            struct #builder_name {
                #builder_fields
            }

            #act_defs

            #[automatically_derived]
            impl rt::__internal::ParserBuilder for #builder_name {
                type Output = #struct_name;
                type Subcommand = #subcommand_ty;

                const SHORT_ARGS: &'static [(rt::__internal::char, rt::__internal::Action<Self>)] =
                    &[#short_args];
                const LONG_ARGS: &'static [(&'static rt::__internal::str, rt::__internal::Action<Self>)] =
                    &[#long_args];

                fn feed_positional(&mut self, __v: rt::__internal::OsString)
                    -> rt::__internal::Result<(), rt::Error>
                {
                    #on_positional
                    rt::__internal::err_extra_positional(__v)
                }

                fn finish(self, __subcmd: rt::__internal::Option<Self::Subcommand>)
                    -> rt::__internal::Result<Self::Output, rt::Error>
                {
                    rt::__internal::Ok(Self::Output { #finish_fields })
                }
            }
        };
    })
}
