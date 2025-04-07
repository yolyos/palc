use std::borrow::Cow;
use std::convert::Infallible;
use std::ffi::{OsStr, OsString};
use std::fmt;
use std::path::PathBuf;

use crate::internal::{ArgsInternal, CommandInternal, try_parse_with_state};

pub use clap_static_derive::{Parser, Subcommand};
use internal::ArgsIter;

mod internal;

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
    pub use {Default, Err, Iterator, None, Ok, Option, Result, Some, char, str, usize};

    pub use crate::internal::{
        ArgsInternal, ArgsIter, CommandInternal, FeedNamed, FeedNamedOk, FeedUnnamed, ParserState,
        try_parse_with_state,
    };
    pub use crate::{Error, FromValue, Parser, Subcommand};

    pub fn extra_positional<T>(arg: OsString) -> Result<T, Error> {
        Err(Error::UnknownArgument(arg))
    }

    pub fn require_subcmd<T>(subcmd: Option<T>) -> Result<T, Error> {
        match subcmd {
            Some(subcmd) => Ok(subcmd),
            None => Err(Error::MissingRequiredSubcommand),
        }
    }

    pub fn take_arg(s: &mut OsString) -> Cow<'static, OsStr> {
        Cow::Owned(std::mem::take(s))
    }
}

#[derive(Debug)]
pub enum Error {
    InvalidUtf8(OsString),
    MissingRequiredArgument(String),
    MissingRequiredSubcommand,
    UnknownArgument(OsString),
    UnexpectedValue(OsString, OsString),
    MissingValue(String),
}

impl std::error::Error for Error {}
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::InvalidUtf8(s) => write!(f, "invalid UTF8: {s:?}"),
            Error::MissingRequiredArgument(name) => write!(f, "missing required argument: {name}"),
            Error::MissingRequiredSubcommand => write!(f, "missing required subcommand"),
            Error::UnknownArgument(s) => write!(f, "unknown argument: {s:?}"),
            Error::UnexpectedValue(name, value) => {
                write!(
                    f,
                    "unexpected value for {value:?} for {}",
                    name.to_string_lossy()
                )
            }
            Error::MissingValue(name) => {
                write!(f, "missing value for {name}")
            }
        }
    }
}

/// Top-level command interface.
pub trait Parser: Sized + 'static + ArgsInternal {
    fn parse() -> Self {
        match Self::try_parse_from(std::env::args_os()) {
            Ok(v) => v,
            Err(_err) => todo!(),
        }
    }

    fn try_parse_from<I, T>(iter: I) -> Result<Self, Error>
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
    fn try_parse_with_name(name: OsString, _args: &mut ArgsIter<'_>) -> Result<Self, Error> {
        Err(Error::UnknownArgument(name))
    }
}

pub trait FromValue: Sized + sealed::Sealed {
    fn try_from_value(v: Cow<'_, OsStr>) -> Result<Self, Error>;
}

mod sealed {
    pub trait Sealed {}
}

impl sealed::Sealed for PathBuf {}
impl FromValue for PathBuf {
    fn try_from_value(v: Cow<'_, OsStr>) -> Result<Self, Error> {
        Ok(v.into_owned().into())
    }
}

impl sealed::Sealed for String {}
impl FromValue for String {
    fn try_from_value(v: Cow<'_, OsStr>) -> Result<Self, Error> {
        v.into_owned().into_string().map_err(Error::InvalidUtf8)
    }
}
