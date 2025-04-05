use std::borrow::Cow;
use std::convert::Infallible;
use std::ffi::{OsStr, OsString};
use std::fmt;
use std::path::PathBuf;

pub use clap_static_derive::{Parser, Subcommand};

#[doc(hidden)]
#[path = "internal.rs"]
pub mod __internal;

#[derive(Debug)]
pub enum Error {
    InvalidUtf8(OsString),
    MissingRequiredArgument(&'static str),
    MissingRequiredSubcommand,
    UnknownArgument(OsString),
    UnexpectedValue(String, OsString),
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
                write!(f, "unexpected value for {value:?} for {name}")
            }
            Error::MissingValue(name) => {
                write!(f, "missing value for {name}")
            }
        }
    }
}

/// Top-level command interface.
pub trait Parser: Sized + 'static + __internal::ParserInternal {
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
        __internal::try_parse_from_builder::<Self::__Builder>(
            &mut iter.into_iter().skip(1).map(|s| s.into()),
        )
    }
}

/// A subcommand enum.
pub trait Subcommand: Sized + 'static + __internal::SubcommandInternal {}

impl Subcommand for Infallible {}
impl __internal::SubcommandInternal for Infallible {
    const __COMMANDS: &'static [&'static str] = &[];

    fn __try_parse_subcommand_from(
        _cmd_idx: usize,
        _iter: &mut dyn Iterator<Item = OsString>,
    ) -> Result<Self, Error> {
        unreachable!()
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
