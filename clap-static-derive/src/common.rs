use darling::util::Override;
use darling::{Error, FromField, FromMeta, FromVariant, Result};
use proc_macro2::TokenStream;
use quote::{ToTokens, quote};
use syn::spanned::Spanned;
use syn::{Attribute, GenericArgument, Ident, PathArguments, Type};

pub const TY_BOOL: &str = "bool";
pub const TY_OPTION: &str = "Option";
pub const TY_VEC: &str = "Vec";

/// `TyCtor<ArgTy>` => `ArgTy`. `ty_ctor` must be a single-identifier path.
pub fn strip_ty_ctor<'i>(mut ty: &'i Type, ty_ctor: &str) -> Option<&'i Type> {
    let ty = loop {
        match ty {
            Type::Group(inner) => ty = &inner.elem,
            Type::Paren(inner) => ty = &inner.elem,
            Type::Path(inner) => break inner,
            _ => return None,
        }
    };
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
            use ::clap_static::__private as __rt;
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
    Vec(&'a Type),
    OptionVec(&'a Type),
}

impl ArgField {
    pub fn arg_ty_kind(&self) -> ArgTyKind<'_> {
        let ty = &self.ty;
        if matches!(ty, Type::Path(p) if p.qself.is_none() && p.path.is_ident(TY_BOOL)) {
            return ArgTyKind::Bool;
        }
        if let Some(subty) = strip_ty_ctor(ty, TY_OPTION) {
            if let Some(subty) = strip_ty_ctor(subty, TY_VEC) {
                return ArgTyKind::OptionVec(subty);
            } else {
                return ArgTyKind::Option(subty);
            }
        }
        if let Some(subty) = strip_ty_ctor(ty, TY_VEC) {
            return ArgTyKind::Vec(subty);
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
            arg = Some(ArgMeta::from_meta(&attr.meta)?);
        } else if path.is_ident("command") {
            if command.is_some() {
                return Err(Error::duplicate_field("command").with_span(&path.span()));
            }
            command = Some((path.span(), CommandMeta::from_meta(&attr.meta)?));
        }
    }
    if let (Some(_), Some((span, _))) = (&arg, &command) {
        return Err(Error::custom("#[arg(..)] conflicts with #[command(..)]").with_span(span));
    }

    if let Some((_, cmd)) = &command {
        if cmd.subcommand {
            return Ok(ArgOrCommand::Subcommand);
        }
        return Ok(ArgOrCommand::Flatten);
    }

    Ok(ArgOrCommand::Arg(match arg {
        Some(arg) if arg.long.is_some() || arg.short.is_some() => Arg::Named(NamedArg {
            long: arg.long,
            short: arg.short,
        }),
        _ => Arg::Unnamed,
    }))
}

pub enum ArgOrCommand {
    Arg(Arg),
    Subcommand,
    Flatten,
}

pub enum Arg {
    Named(NamedArg),
    Unnamed,
}

pub struct NamedArg {
    pub long: Option<Override<String>>,
    pub short: Option<Override<char>>,
}

#[derive(FromMeta)]
#[cfg_attr(feature = "__test-allow-unknown-fields", darling(allow_unknown_fields))]
struct ArgMeta {
    long: Option<Override<String>>,
    short: Option<Override<char>>,
}

#[derive(FromMeta)]
#[darling(and_then = CommandMeta::validate)]
pub struct CommandMeta {
    #[darling(default)]
    pub subcommand: bool,
    #[darling(default)]
    pub flatten: bool,
}

impl CommandMeta {
    fn validate(self) -> Result<Self> {
        if !self.subcommand && !self.flatten {
            return Err(Error::custom("empty `#[command(..)]`"));
        }
        if self.subcommand && self.flatten {
            return Err(Error::custom(
                "`#[command(subcommand)]` conflicts with `#[command(flatten)]`",
            ));
        }
        Ok(self)
    }
}
