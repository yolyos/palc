use darling::util::Override;
use darling::{Error, FromField, FromMeta, FromVariant, Result};
use proc_macro2::TokenStream;
use quote::{ToTokens, quote};
use syn::spanned::Spanned;
use syn::{Attribute, GenericArgument, Ident, PathArguments, Type};

pub const TY_BOOL: &str = "bool";
pub const TY_OPTION: &str = "Option";

/// `TyCtor<ArgTy>` => `ArgTy`. `ty_ctor` must be a single-identifier path.
pub fn strip_ty_ctor<'i>(ty: &'i Type, ty_ctor: &str) -> Option<&'i Type> {
    if let Type::Path(ty) = ty {
        if ty.path.leading_colon.is_none() && ty.path.segments.len() == 1 {
            let seg = &ty.path.segments[0];
            if seg.ident == ty_ctor {
                if let PathArguments::AngleBracketed(args) = &seg.arguments {
                    if args.args.len() == 1 {
                        if let GenericArgument::Type(arg_ty) = &args.args[0] {
                            return Some(arg_ty);
                        }
                    }
                }
            }
        }
    }
    None
}

#[derive(FromVariant)]
pub struct CommandVariant {
    pub ident: Ident,
    pub fields: darling::ast::Fields<ArgField>,
}

pub fn wrap_anon_item(tts: impl ToTokens) -> TokenStream {
    quote! {
        const _: () = {
            extern crate clap_static as rt;
            #tts
        };
    }
}

#[derive(FromField)]
#[darling(forward_attrs(arg, command))]
pub struct ArgField {
    pub ident: Option<Ident>,
    pub ty: Type,

    #[darling(with = parse_arg_or_command)]
    pub attrs: ArgOrCommand,
}

pub enum ArgTyKind<'a> {
    Bool,
    Option(&'a Type),
    Convert,
}

impl ArgField {
    pub fn arg_ty_kind(&self) -> ArgTyKind<'_> {
        let ty = &self.ty;
        if matches!(ty, Type::Path(p) if p.qself.is_none() && p.path.is_ident(TY_BOOL)) {
            return ArgTyKind::Bool;
        }
        if let Some(arg_ty) = strip_ty_ctor(ty, TY_OPTION) {
            return ArgTyKind::Option(arg_ty);
        }
        ArgTyKind::Convert
    }
}

fn parse_arg_or_command(attrs: Vec<Attribute>) -> Result<ArgOrCommand> {
    let mut arg = None;
    let mut command = None;
    for attr in attrs {
        let path = attr.path();
        if path.is_ident("arg") {
            if arg.is_some() {
                return Err(Error::duplicate_field("arg").with_span(&path.span()));
            }
            arg = Some(Arg::from_meta(&attr.meta)?);
        } else if path.is_ident("command") {
            if command.is_some() {
                return Err(Error::duplicate_field("command").with_span(&path.span()));
            }
            command = Some((path.span(), Command::from_meta(&attr.meta)?));
        }
    }
    match (arg, command) {
        (None, None) => Ok(ArgOrCommand::None),
        (Some(arg), None) => Ok(ArgOrCommand::Arg(arg)),
        (None, Some((_, cmd))) => Ok(ArgOrCommand::Command(cmd)),
        (Some(_), Some((span, _))) => {
            Err(Error::custom("#[arg(..)] conflicts with #[command(..)]").with_span(&span))
        }
    }
}

pub enum ArgOrCommand {
    None,
    Arg(Arg),
    Command(Command),
}

#[derive(FromMeta)]
#[darling(and_then = Arg::validate)]
pub struct Arg {
    pub long: Option<Override<String>>,
    pub short: Option<Override<char>>,
}

impl Arg {
    fn validate(self) -> Result<Self> {
        if self.long.is_none() && self.short.is_none() {
            return Err(Error::custom("no-op `#[arg(..)]`"));
        }
        Ok(self)
    }
}

#[derive(FromMeta)]
#[darling(and_then = Command::validate)]
pub struct Command {
    pub subcommand: bool,
}

impl Command {
    fn validate(self) -> Result<Self> {
        if !self.subcommand {
            return Err(Error::custom("no-op `#[subcommand(..)]`"));
        }
        Ok(self)
    }
}
