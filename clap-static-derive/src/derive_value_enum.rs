use darling::{FromDeriveInput, FromVariant};
use proc_macro2::TokenStream;
use quote::{ToTokens, quote};
use syn::{DeriveInput, Ident};

use crate::common::wrap_anon_item;

pub(crate) fn expand(input: DeriveInput) -> TokenStream {
    match ValueEnumDef::from_derive_input(&input) {
        Ok(def) => match expand_impl(def) {
            Ok(tts) => wrap_anon_item(tts),
            Err(err) => err.to_compile_error(),
        },
        Err(err) => err.write_errors(),
    }
}

#[derive(FromDeriveInput)]
#[darling(supports(enum_unit))]
struct ValueEnumDef {
    ident: Ident,
    data: darling::ast::Data<ValueEnumVariant, ()>,
}

#[derive(FromVariant)]
struct ValueEnumVariant {
    ident: Ident,
}

fn expand_impl(def: ValueEnumDef) -> syn::Result<ValueEnumImpl> {
    let variants = def.data.take_enum().expect("validated by darling");
    let mut variants = variants
        .into_iter()
        .map(|variant| {
            let arg_name = heck::AsKebabCase(variant.ident.to_string()).to_string();
            (arg_name, variant.ident)
        })
        .collect::<Vec<_>>();

    variants.sort_by(|lhs, rhs| Ord::cmp(&lhs.0, &rhs.0));
    if let Some(w) = variants.windows(2).find(|w| w[0].0 == w[1].0) {
        let mut err = syn::Error::new(w[0].1.span(), "duplicated variant names after renaming");
        err.combine(syn::Error::new(w[1].1.span(), "second variant here"));
        return Err(err);
    }

    Ok(ValueEnumImpl {
        ident: def.ident,
        variants,
    })
}

struct ValueEnumImpl {
    ident: Ident,
    variants: Vec<(String, Ident)>,
}

impl ToTokens for ValueEnumImpl {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = &self.ident;
        let variant_strs = self.variants.iter().map(|(s, _)| s);
        let variant_names = self.variants.iter().map(|(_, name)| name);
        tokens.extend(quote! {
            #[automatically_derived]
            impl __rt::ArgValue for #name {
                // If this enum has no variants.
                #[allow(unreachable_code)]
                const INFO: __rt::ArgValueInfo<#name> = __rt::ArgValueInfo {
                    parser: |__v| {
                        __rt::Ok(match __rt::str_from_utf8(&__v)? {
                            #(#variant_strs => #name :: #variant_names,)*
                            __s => return __rt::invalid_value(__s)
                        })
                    },
                };
            }
        });
    }
}
