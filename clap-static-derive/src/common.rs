use darling::FromField;
use darling::util::Override;
use proc_macro2::TokenStream;
use quote::{ToTokens, quote};
use syn::{GenericArgument, Ident, PathArguments, Type};

pub fn wrap_anon_item(tts: impl ToTokens) -> TokenStream {
    quote! {
        const _: () = {
            extern crate clap_static as rt;
            #tts
        };
    }
}

#[derive(FromField)]
#[darling(attributes(arg))]
pub struct ArgField {
    pub ident: Option<Ident>,
    pub ty: Type,

    pub long: Option<Override<String>>,
    pub short: Option<Override<char>>,
}

pub enum ArgTyKind<'a> {
    Bool,
    Option(&'a Type),
    Convert,
}

impl ArgField {
    pub fn arg_ty_kind(&self) -> ArgTyKind<'_> {
        let ty = match &self.ty {
            Type::Path(ty) if ty.qself.is_none() => ty,
            _ => return ArgTyKind::Convert,
        };
        if ty.path.is_ident("bool") {
            return ArgTyKind::Bool;
        }
        if ty.path.leading_colon.is_none() && ty.path.segments.len() == 1 {
            let seg = &ty.path.segments[0];
            if seg.ident == "Option" {
                if let PathArguments::AngleBracketed(args) = &seg.arguments {
                    if args.args.len() == 1 {
                        if let GenericArgument::Type(arg_ty) = &args.args[0] {
                            return ArgTyKind::Option(arg_ty);
                        }
                    }
                }
            }
        }
        ArgTyKind::Convert
    }
}
