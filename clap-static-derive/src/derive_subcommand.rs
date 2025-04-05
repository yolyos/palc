use darling::FromDeriveInput;
use heck::ToKebabCase;
use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, format_ident, quote};
use syn::{DeriveInput, Generics, Ident};

use crate::common::{CommandVariant, wrap_anon_item};
use crate::derive_parser::{BuilderImpl, ParserItemDef};

#[derive(FromDeriveInput)]
#[darling(supports(enum_named))]
struct SubcommandDef {
    ident: Ident,
    generics: Generics,
    data: darling::ast::Data<CommandVariant, ()>,
}

pub(crate) fn expand(input: DeriveInput) -> TokenStream {
    match SubcommandDef::from_derive_input(&input) {
        Err(err) => err.write_errors(),
        Ok(def) => match expand_impl(def) {
            Ok(tts) => wrap_anon_item(tts),
            Err(err) => err.into_compile_error(),
        },
    }
}

struct SubcommandImpl {
    enum_name: Ident,
    variant_builder_impls: Vec<BuilderImpl>,
    variant_infos: Vec<(String, usize)>,
}

fn expand_impl(def: SubcommandDef) -> syn::Result<SubcommandImpl> {
    let variants = def.data.take_enum().expect("checked by darling");

    if !def.generics.params.is_empty() || def.generics.where_clause.is_some() {
        return Err(syn::Error::new(
            def.ident.span(),
            "TODO: generics are not supported yet",
        ));
    }

    let enum_name = def.ident;

    let mut variant_builder_impls = Vec::new();
    let mut variant_infos = Vec::new();

    for (idx, variant) in variants.into_iter().enumerate() {
        let variant_name = variant.ident;

        let mut builder_impl = crate::derive_parser::expand_impl(ParserItemDef {
            ident: format_ident!("{enum_name}{variant_name}", span = Span::call_site()),
            generics: Default::default(),
            data: darling::ast::Data::Struct(variant.fields),
        })?
        .builder;
        builder_impl.struct_name = enum_name.clone();
        builder_impl.struct_ctor = quote!(#enum_name :: #variant_name);

        variant_builder_impls.push(builder_impl);
        variant_infos.push((variant_name.to_string().to_kebab_case(), idx));
    }

    variant_infos.sort_by(|lhs, rhs| Ord::cmp(&lhs.0, &rhs.0));
    Ok(SubcommandImpl {
        enum_name,
        variant_builder_impls,
        variant_infos,
    })
}

impl ToTokens for SubcommandImpl {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            enum_name,
            variant_builder_impls,
            variant_infos,
        } = self;

        let variant_names = variant_infos.iter().map(|(s, _)| s);
        let builder_names = variant_builder_impls.iter().map(|i| &i.builder_name);
        let idx = variant_infos.iter().map(|(_, idx)| *idx);

        tokens.extend(quote! {
            #(#variant_builder_impls)*

            impl rt::Subcommand for #enum_name {}
            impl rt::__internal::SubcommandInternal for #enum_name {
                const __COMMANDS: &'static [&'static rt::__internal::str] = &[#(#variant_names),*];

                fn __try_parse_subcommand_from(
                    __cmd_idx: rt::__internal::usize,
                    __iter: &mut dyn rt::__internal::Iterator<Item = rt::__internal::OsString>,
                ) -> rt::__internal::Result<Self, rt::Error> {
                    match __cmd_idx {
                        #(#idx => rt::__internal::try_parse_from_builder::<#builder_names>(__iter),)*
                        _ => rt::__internal::unreachable!(),
                    }
                }
            }
        });
    }
}
