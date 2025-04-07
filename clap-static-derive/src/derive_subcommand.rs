use darling::FromDeriveInput;
use heck::ToKebabCase;
use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, format_ident, quote};
use syn::{DeriveInput, Generics, Ident, LitByteStr};

use crate::common::{CommandVariant, wrap_anon_item};
use crate::derive_parser::ParserStateDefImpl;

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
    variants: Vec<(String, Span, ParserStateDefImpl)>,
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

    let variants = input_variants
        .into_iter()
        .map(|variant| {
            let variant_name = variant.ident;
            let mut state = crate::derive_parser::expand_state_def_impl(
                format_ident!("{enum_name}{variant_name}State", span = variant_name.span()),
                enum_name.to_token_stream(),
                &variant.fields.fields,
            )?;
            state.output_ctor = Some(quote!(#enum_name :: #variant_name));
            Ok((
                variant_name.to_string().to_kebab_case(),
                variant_name.span(),
                state,
            ))
        })
        .collect::<syn::Result<Vec<_>>>()?;

    Ok(SubcommandImpl {
        enum_name,
        variants,
    })
}

impl ToTokens for SubcommandImpl {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            enum_name,
            variants,
        } = self;
        let name_byte_strs = variants
            .iter()
            .map(|(name, span, _)| LitByteStr::new(name.as_bytes(), *span));
        let states = variants.iter().map(|(_, _, state)| state);
        let state_names = variants.iter().map(|(_, _, state)| &state.state_name);

        tokens.extend(quote! {
            #(#states)*

            impl __rt::Subcommand for #enum_name {}
            impl __rt::CommandInternal for #enum_name {
                fn try_parse_with_name(
                    __name: __rt::OsString,
                    __args: &mut __rt::ArgsIter<'_>,
                ) -> __rt::Result<Self, __rt::Error> {
                    match __rt::OsStr::as_encoded_bytes(&__name) {
                        #(#name_byte_strs => __rt::try_parse_with_state::<#state_names>(__args),)*
                        _ => __rt::extra_positional(__name),
                    }
                }
            }
        });
    }
}
