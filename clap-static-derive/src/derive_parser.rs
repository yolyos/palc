use darling::FromDeriveInput;
use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, format_ident, quote};
use syn::{DeriveInput, Generics, Ident};

use crate::common::{ArgField, ArgTyKind, wrap_anon_item};

#[derive(FromDeriveInput)]
#[darling(supports(struct_named))]
struct ParserItemDef {
    ident: Ident,
    generics: Generics,
    data: darling::ast::Data<(), ArgField>,
}

pub(crate) fn expand(input: DeriveInput) -> TokenStream {
    match ParserItemDef::from_derive_input(&input) {
        Err(err) => err.write_errors(),
        Ok(def) => match expand_impl(def) {
            Ok(tts) => wrap_anon_item(tts),
            Err(err) => err.into_compile_error(),
        },
    }
}

struct ParserImpl {
    struct_name: Ident,
    builder_name: Ident,
    builder_fields: Vec<Ident>,
    builder_field_tys: Vec<TokenStream>,
    on_positional: TokenStream,
    act_defs: Vec<(Ident, TokenStream)>,
    long_args: Vec<(String, Ident)>,
    short_args: Vec<(char, Ident)>,
    finish_exprs: Vec<TokenStream>,
}

fn expand_impl(def: ParserItemDef) -> syn::Result<ParserImpl> {
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

    let mut on_positional = TokenStream::new();

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
        struct_name,
        builder_name,
        builder_fields,
        builder_field_tys,
        on_positional,
        act_defs,
        long_args,
        short_args,
        finish_exprs,
    })
}

impl ToTokens for ParserImpl {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            struct_name,
            builder_name,
            builder_fields,
            builder_field_tys,
            finish_exprs,
            on_positional,
            ..
        } = self;

        let act_name = self.act_defs.iter().map(|(name, _)| name);
        let act_expr = self.act_defs.iter().map(|(_, expr)| expr);
        let short_ch = self.short_args.iter().map(|(ch, _)| *ch);
        let short_act = self.short_args.iter().map(|(_, act)| act);
        let long_str = self.long_args.iter().map(|(s, _)| s.as_str());
        let long_act = self.long_args.iter().map(|(_, act)| act);

        tokens.extend(quote! {
            #[automatically_derived]
            impl rt::Parser for #struct_name {}

            #[automatically_derived]
            impl rt::__internal::ParserInternal for #struct_name {
                type __Builder = #builder_name;
            }

            #[derive(Default)]
            struct #builder_name {
                #(#builder_fields : #builder_field_tys,)*
            }

            #(const #act_name : rt::__internal::Action<#builder_name> =
                rt::__internal::Action:: #act_expr;)*

            #[automatically_derived]
            impl rt::__internal::ParserBuilder for #builder_name {
                type Output = #struct_name;
                type Subcommand = rt::__internal::Infallible;

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
                    rt::__internal::Ok(Self::Output {
                        #(#builder_fields : #finish_exprs,)*
                    })
                }
            }
        });
    }
}
