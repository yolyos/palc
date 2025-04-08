use std::borrow::Cow;
use std::convert::Infallible;
use std::ffi::{OsStr, OsString};
use std::path::PathBuf;

use crate::internal::{ArgsInternal, CommandInternal, try_parse_with_state};

pub use clap_static_derive::{Parser, Subcommand};
use error::ErrorKind;
use internal::ArgsIter;

mod error;
mod internal;

pub use crate::error::Error;
pub type Result<T, E = Error> = std::result::Result<T, E>;

/// Not public API. Only for proc-macro internal use.
// To scan all usages:
// ```sh
// rg --only-matching --no-filename '\b__rt::\w+' | LC_COLLATE=C sort --unique
// ```
#[doc(hidden)]
pub mod __private {
    pub use std::borrow::Cow;
    pub use std::convert::Infallible;
    pub use std::ffi::{OsStr, OsString};
    pub use {Default, Err, Iterator, None, Ok, Option, Some, char, str, usize};

    use crate::ErrorKind;
    pub use crate::internal::{
        ArgsInternal, ArgsIter, CommandInternal, FeedNamed, FeedNamedOk, FeedUnnamed, ParserState,
        try_parse_with_state,
    };
    pub use crate::{FromValue, Parser, Result, Subcommand};

    pub fn unknown_subcommand<T>(arg: &str) -> Result<T> {
        Err(ErrorKind::UnknownSubcommand(arg.into()).into())
    }

    pub fn missing_required_arg<T>(arg: &'static str) -> Result<T> {
        Err(ErrorKind::MissingRequiredArgument.with_arg(arg))
    }

    pub fn missing_required_subcmd<T>() -> Result<T> {
        Err(ErrorKind::MissingRequiredSubcommand.into())
    }

    pub fn take_arg(s: &mut OsString) -> Cow<'static, OsStr> {
        Cow::Owned(std::mem::take(s))
    }
}

/// Top-level command interface.
pub trait Parser: Sized + 'static + ArgsInternal {
    fn parse() -> Self {
        match Self::try_parse_from(std::env::args_os()) {
            Ok(v) => v,
            Err(err) => {
                eprintln!("error: {err}");
                std::process::exit(1);
            }
        }
    }

    fn try_parse_from<I, T>(iter: I) -> Result<Self>
    where
        I: IntoIterator<Item = T>,
        T: Into<OsString> + Clone,
    {
        let mut iter = iter.into_iter().skip(1).map(|s| s.into());
        let mut args = ArgsIter::new(&mut iter);
        try_parse_with_state::<Self::__State>(&mut args)
    }
}

/// A subcommand enum.
pub trait Subcommand: Sized + 'static + CommandInternal {}

impl Subcommand for Infallible {}
impl CommandInternal for Infallible {
    fn try_parse_with_name(name: &str, _args: &mut ArgsIter<'_>) -> Result<Self> {
        Err(ErrorKind::UnexpectedUnnamedArgument(name.into()).into())
    }
}

pub trait FromValue: Sized + sealed::Sealed {
    fn try_from_value(v: Cow<'_, OsStr>) -> Result<Self>;
}

mod sealed {
    pub trait Sealed {}
}

impl sealed::Sealed for PathBuf {}
impl FromValue for PathBuf {
    fn try_from_value(v: Cow<'_, OsStr>) -> Result<Self> {
        Ok(v.into_owned().into())
    }
}

impl sealed::Sealed for String {}
impl FromValue for String {
    fn try_from_value(v: Cow<'_, OsStr>) -> Result<Self> {
        v.into_owned()
            .into_string()
            .map_err(|s| ErrorKind::InvalidUtf8(s).into())
    }
}

impl sealed::Sealed for f64 {}
impl FromValue for f64 {
    fn try_from_value(s: Cow<'_, OsStr>) -> Result<Self> {
        let s = s
            .to_str()
            .ok_or_else(|| ErrorKind::InvalidUtf8(s.to_os_string()))?;
        s.parse::<f64>()
            .map_err(|err| ErrorKind::InvalidValue(s.into(), err.to_string()).into())
    }
}

impl sealed::Sealed for usize {}
impl FromValue for usize {
    fn try_from_value(s: Cow<'_, OsStr>) -> Result<Self> {
        let s = s
            .to_str()
            .ok_or_else(|| ErrorKind::InvalidUtf8(s.to_os_string()))?;
        s.parse::<usize>()
            .map_err(|err| ErrorKind::InvalidValue(s.into(), err.to_string()).into())
    }
}
