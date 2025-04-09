use darling::FromDeriveInput;
use darling::ast::Style;
use proc_macro2::TokenStream;
use quote::{ToTokens, format_ident, quote};
use syn::{DeriveInput, Generics, Ident, Type, Visibility};

use crate::common::{CommandVariant, wrap_anon_item};
use crate::derive_args::ParserStateDefImpl;

#[derive(FromDeriveInput)]
#[darling(supports(enum_any))]
struct SubcommandDef {
    vis: Visibility,
    ident: Ident,
    generics: Generics,
    data: darling::ast::Data<CommandVariant, ()>,
}

pub(crate) fn expand(input: DeriveInput) -> TokenStream {
    let mut tts = match SubcommandDef::from_derive_input(&input) {
        Err(err) => err.write_errors(),
        Ok(def) => match expand_impl(def) {
            Ok(tts) => return wrap_anon_item(tts),
            Err(err) => err.into_compile_error(),
        },
    };

    let name = &input.ident;
    tts.extend(wrap_anon_item(quote! {
        impl __rt::Subcommand for #name {}
        impl __rt::CommandInternal for #name {
            fn try_parse_with_name(
                __name: &__rt::str,
                __args: &mut __rt::ArgsIter<'_>,
            ) -> __rt::Result<Self> {
                __rt::unimplemented!()
            }
        }
    }));
    tts
}

struct SubcommandImpl {
    enum_name: Ident,
    state_defs: Vec<ParserStateDefImpl>,
    variants: Vec<(String, VariantImpl)>,
}

enum VariantImpl {
    Unit { variant_name: Ident },
    Tuple { variant_name: Ident, ty: Type },
    Struct { state_name: Ident },
}

impl ToTokens for VariantImpl {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(match self {
            VariantImpl::Unit { variant_name } => quote! {
                __rt::Ok(Self::#variant_name)
            },
            VariantImpl::Tuple { variant_name, ty } => quote! {
                __rt::try_parse_with_state::<<#ty as __rt::ArgsInternal>::__State>(__args).map(Self::#variant_name)
            },
            VariantImpl::Struct { state_name } => quote! {
                __rt::try_parse_with_state::<#state_name>(__args)
            },
        })
    }
}

fn expand_impl(def: SubcommandDef) -> syn::Result<SubcommandImpl> {
    let input_variants = def.data.take_enum().expect("checked by darling");

    if !def.generics.params.is_empty() || def.generics.where_clause.is_some() {
        return Err(syn::Error::new(
            def.ident.span(),
            "TODO: generics are not supported yet",
        ));
    }

    let enum_name = def.ident;
    let mut state_defs = Vec::with_capacity(input_variants.len());

    let variants = input_variants
        .into_iter()
        .map(|mut variant| {
            let variant_name = variant.ident;
            let arg_name = heck::AsKebabCase(variant_name.to_string()).to_string();
            let act = match variant.fields.style {
                Style::Unit => VariantImpl::Unit { variant_name },
                Style::Tuple if variant.fields.fields.len() == 1 => VariantImpl::Tuple {
                    variant_name,
                    ty: variant.fields.fields.pop().unwrap().ty,
                },
                Style::Tuple => {
                    return Err(syn::Error::new(
                        variant_name.span(),
                        "non-1-element tuple variant is not supported",
                    ));
                }
                Style::Struct => {
                    let state_name =
                        format_ident!("{enum_name}{variant_name}State", span = variant_name.span());
                    let mut state = crate::derive_args::expand_state_def_impl(
                        def.vis.clone(),
                        state_name.clone(),
                        enum_name.to_token_stream(),
                        &variant.fields.fields,
                    )?;
                    state.output_ctor = Some(quote!(#enum_name :: #variant_name));
                    state_defs.push(state);
                    VariantImpl::Struct { state_name }
                }
            };
            Ok((arg_name, act))
        })
        .collect::<syn::Result<Vec<_>>>()?;

    Ok(SubcommandImpl {
        enum_name,
        state_defs,
        variants,
    })
}

impl ToTokens for SubcommandImpl {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            enum_name,
            state_defs,
            variants,
        } = self;
        let name_strs = variants.iter().map(|(arg_name, _)| arg_name);
        let cases = variants.iter().map(|(_, e)| e);

        tokens.extend(quote! {
            #(#state_defs)*

            impl __rt::Subcommand for #enum_name {}
            impl __rt::CommandInternal for #enum_name {
                fn try_parse_with_name(
                    __name: &__rt::str,
                    __args: &mut __rt::ArgsIter<'_>,
                ) -> __rt::Result<Self> {
                    match __name {
                        #(#name_strs => #cases,)*
                        _ => __rt::unknown_subcommand(__name),
                    }
                }
            }
        });
    }
}
