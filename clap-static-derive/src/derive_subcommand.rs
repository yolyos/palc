use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, format_ident, quote};
use syn::{DeriveInput, Ident, Type};

use crate::common::{ErrorCollector, TopCommandMeta, wrap_anon_item};
use crate::derive_args::ParserStateDefImpl;

pub(crate) fn expand(input: DeriveInput) -> TokenStream {
    let mut tts = match expand_impl(&input) {
        Ok(out) => return wrap_anon_item(out),
        Err(err) => err.into_compile_error(),
    };

    let name = &input.ident;
    tts.extend(wrap_anon_item(quote! {
        impl __rt::Subcommand for #name {}
        impl __rt::CommandInternal for #name {
            const COMMAND_INFO: __rt::CommandInfo = __rt::CommandInfo::__new(&[]);
            fn try_parse_with_name(
                __name: &__rt::str,
                __args: &mut __rt::ArgsIter<'_>,
                __global: __rt::GlobalAncestors<'_>,
            ) -> __rt::Result<Self> {
                __rt::unimplemented!()
            }
        }
    }));
    tts
}

struct SubcommandImpl<'i> {
    enum_name: &'i Ident,
    state_defs: Vec<ParserStateDefImpl<'i>>,
    variants: Vec<(String, VariantImpl<'i>)>,
}

enum VariantImpl<'i> {
    Unit {
        variant_name: &'i Ident,
    },
    Tuple {
        variant_name: &'i Ident,
        ty: &'i Type,
    },
    Struct {
        state_name: Ident,
    },
}

impl ToTokens for VariantImpl<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(match self {
            VariantImpl::Unit { variant_name } => quote! {
                __rt::Ok(Self::#variant_name)
            },
            VariantImpl::Tuple { variant_name, ty } => quote! {
                __rt::try_parse_args::<#ty>(__args, __global).map(Self::#variant_name)
            },
            VariantImpl::Struct { state_name } => quote! {{
                let mut __state = <#state_name as __rt::ParserState>::init();
                __rt::try_parse_with_state(&mut __state, __args, __global)?;
                __rt::ParserState::finish(__state)
            }},
        })
    }
}

fn expand_impl(def: &DeriveInput) -> syn::Result<SubcommandImpl<'_>> {
    let syn::Data::Enum(data) = &def.data else {
        return Err(syn::Error::new(
            Span::call_site(),
            "derive(Subcommand) can only be used on enums",
        ));
    };

    if !def.generics.params.is_empty() || def.generics.where_clause.is_some() {
        return Err(syn::Error::new(
            def.ident.span(),
            "TODO: generics are not supported yet",
        ));
    }

    let mut errs = ErrorCollector::default();
    let enum_name = &def.ident;
    let mut state_defs = Vec::with_capacity(data.variants.len());

    let variants = data
        .variants
        .iter()
        .filter_map(|variant| {
            let variant_name = &variant.ident;
            let arg_name = heck::AsKebabCase(variant_name.to_string()).to_string();
            let act = match &variant.fields {
                syn::Fields::Unit => VariantImpl::Unit { variant_name },
                syn::Fields::Unnamed(fields) => {
                    if fields.unnamed.len() != 1 {
                        errs.push(syn::Error::new(
                            variant_name.span(),
                            "subcommand tuple variant must have a single element",
                        ));
                        return None;
                    }
                    VariantImpl::Tuple {
                        variant_name,
                        ty: &fields.unnamed[0].ty,
                    }
                }
                syn::Fields::Named(fields) => {
                    let state_name =
                        format_ident!("{enum_name}{variant_name}State", span = variant_name.span());
                    // TODO: Should this also include enum attributes?
                    let cmd_meta = errs.collect(TopCommandMeta::parse_attrs(&variant.attrs))?;
                    let mut state = errs.collect(crate::derive_args::expand_state_def_impl(
                        &def.vis,
                        Some(cmd_meta),
                        state_name.clone(),
                        enum_name.to_token_stream(),
                        fields,
                    ))?;
                    state.output_ctor = Some(quote!(#enum_name :: #variant_name));
                    state_defs.push(state);
                    VariantImpl::Struct { state_name }
                }
            };
            Some((arg_name, act))
        })
        .collect::<Vec<_>>();

    errs.finish_then(SubcommandImpl {
        enum_name,
        state_defs,
        variants,
    })
}

impl ToTokens for SubcommandImpl<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            enum_name,
            state_defs,
            variants,
        } = self;
        let name_strs = variants.iter().map(|(arg_name, _)| arg_name);
        let cases = variants.iter().map(|(_, e)| e);
        let command_info_lit = CommandInfoLiteral(self);

        tokens.extend(quote! {
            #(#state_defs)*

            impl __rt::Subcommand for #enum_name {}
            impl __rt::CommandInternal for #enum_name {
                const COMMAND_INFO: __rt::CommandInfo = #command_info_lit;

                fn try_parse_with_name(
                    __name: &__rt::str,
                    __args: &mut __rt::ArgsIter<'_>,
                    __global: __rt::GlobalAncestors<'_>,
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

struct CommandInfoLiteral<'i>(&'i SubcommandImpl<'i>);

impl ToTokens for CommandInfoLiteral<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let SubcommandImpl { variants, .. } = self.0;
        let subcmd_names = variants.iter().map(|(s, _)| s);
        let subargs = variants.iter().map(|(_, variant)| match variant {
            VariantImpl::Unit { .. } => quote! { __rt::ArgsInfo::empty() },
            VariantImpl::Tuple { ty, .. } => {
                quote! { <<#ty as __rt::ArgsInternal>::__State as __rt::ParserState>::ARGS_INFO }
            }
            VariantImpl::Struct { state_name } => {
                quote! { <#state_name as __rt::ParserState>::ARGS_INFO }
            }
        });

        tokens.extend(quote! {
            __rt::CommandInfo::__new(&[
                #((#subcmd_names, &#subargs)),*
            ])
        });
    }
}
