use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, quote};
use syn::{DeriveInput, Generics, Ident};

use crate::common::wrap_anon_item;

pub(crate) fn expand(input: &DeriveInput) -> TokenStream {
    let mut tts = match expand_impl(input) {
        Ok(tts) => return wrap_anon_item(tts),
        Err(err) => err.to_compile_error(),
    };

    tts.extend(wrap_anon_item(ValueEnumImpl {
        ident: &input.ident,
        generics: &input.generics,
        variants: Vec::new(),
    }));
    tts
}

fn expand_impl(def: &DeriveInput) -> syn::Result<ValueEnumImpl<'_>> {
    let syn::Data::Enum(enum_def) = &def.data else {
        return Err(syn::Error::new(
            Span::call_site(),
            "derive(ValueEnum) can only be used on enums",
        ));
    };

    let mut variants = enum_def
        .variants
        .iter()
        .map(|variant| {
            if !matches!(variant.fields, syn::Fields::Unit) {
                return Err(syn::Error::new(
                    variant.ident.span(),
                    "only unit variant is supported",
                ));
            }
            let arg_name = heck::AsKebabCase(variant.ident.to_string()).to_string();
            Ok((arg_name, &variant.ident))
        })
        .collect::<syn::Result<Vec<_>>>()?;

    variants.sort_by(|lhs, rhs| Ord::cmp(&lhs.0, &rhs.0));
    if let Some(w) = variants.windows(2).find(|w| w[0].0 == w[1].0) {
        let mut err = syn::Error::new(w[0].1.span(), "duplicated variant names after renaming");
        err.combine(syn::Error::new(w[1].1.span(), "second variant here"));
        return Err(err);
    }

    Ok(ValueEnumImpl { ident: &def.ident, generics: &def.generics, variants })
}

struct ValueEnumImpl<'i> {
    ident: &'i Ident,
    generics: &'i Generics,
    variants: Vec<(String, &'i Ident)>,
}

impl ToTokens for ValueEnumImpl<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = self.ident;
        let (impl_generics, ty_generics, where_clause) = self.generics.split_for_impl();
        let variant_strs = self.variants.iter().map(|(s, _)| s);
        let variant_names = self.variants.iter().map(|(_, name)| name);

        if self.variants.is_empty() {
            tokens.extend(quote! {
                #[automatically_derived]
                impl #impl_generics __rt::ValueEnum for #name #ty_generics #where_clause {
                    fn parse_value(_: &__rt::str) -> __rt::Option<Self> {
                        __rt::None
                    }
                }
            });
            return;
        }

        tokens.extend(quote! {
            #[automatically_derived]
            impl #impl_generics __rt::ValueEnum for #name #ty_generics #where_clause {
                fn parse_value(__v: &__rt::str) -> __rt::Option<Self> {
                    __rt::Some(match __v {
                        #(#variant_strs => Self:: #variant_names,)*
                        _ => return __rt::None
                    })
                }
            }
        });
    }
}
