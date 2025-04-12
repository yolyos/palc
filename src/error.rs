use std::ffi::OsString;
use std::fmt;

use crate::internal::CommandInternal;

#[derive(Debug)]
pub struct Error(Box<Inner>);

#[derive(Debug)]
struct Inner {
    kind: ErrorKind,
    arg: Option<Arg>,

    #[cfg(feature = "help")]
    subcommand_path: crate::help::SubcommandPath,
}

#[derive(Debug)]
pub(crate) enum ErrorKind {
    InvalidUtf8(OsString),
    MissingRequiredArgument,
    MissingRequiredSubcommand,
    UnknownNamedArgument,
    UnknownSubcommand(OsString),
    UnexpectedUnnamedArgument(OsString),
    UnexpectedValue(OsString),
    MissingValue,
    InvalidValue(OsString, String),
    MissingArg0,
    MissingEq,

    #[cfg(feature = "help")]
    Help,

    Custom(String),
}

#[derive(Debug)]
struct Arg(OsString);

impl fmt::Display for Arg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Short arguments have their dash stripped.
        if !self.0.as_encoded_bytes().starts_with(b"-") {
            f.write_str("-")?;
        }
        f.write_str(&self.0.to_string_lossy())
    }
}

impl std::error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        struct MaybeArg<'a>(&'static str, Option<&'a Arg>);

        impl fmt::Display for MaybeArg<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                if let Some(arg) = &self.1 {
                    f.write_str(self.0)?;
                    arg.fmt(f)?;
                }
                Ok(())
            }
        }

        let mut maybe_arg = MaybeArg(" for ", self.0.arg.as_ref());

        match &self.0.kind {
            ErrorKind::InvalidUtf8(s) => write!(f, "invalid UTF8 {s:?}{maybe_arg}"),
            ErrorKind::MissingRequiredArgument => {
                maybe_arg.0 = " ";
                write!(f, "missing required argument{maybe_arg}")
            }
            ErrorKind::MissingRequiredSubcommand => write!(f, "missing required subcommand"),
            ErrorKind::UnknownNamedArgument => {
                maybe_arg.0 = " ";
                write!(f, "unknown argument{maybe_arg}")
            }
            ErrorKind::UnknownSubcommand(subcmd) => {
                write!(f, "unknown subcommand {subcmd:?}")
            }
            ErrorKind::UnexpectedUnnamedArgument(arg) => {
                write!(f, "unexpected argument {arg:?}")
            }
            ErrorKind::UnexpectedValue(value) => {
                write!(f, "unexpected value {value:?}{maybe_arg}")
            }
            ErrorKind::MissingValue => {
                write!(f, "missing value{maybe_arg}")
            }
            ErrorKind::InvalidValue(value, err) => {
                write!(f, "invalid value {value:?}{maybe_arg}: {err}")
            }
            ErrorKind::MissingArg0 => {
                write!(f, "missing executable argument (argv[0])")
            }
            ErrorKind::MissingEq => {
                maybe_arg.0 = " ";
                write!(f, "missing required '=' for argument{maybe_arg}")
            }

            #[cfg(feature = "help")]
            ErrorKind::Help => {
                let mut out = String::new();
                crate::help::generate(&self.0.subcommand_path, &mut out).unwrap();
                f.write_str(&out)
            }

            ErrorKind::Custom(reason) => {
                if let Some(arg) = &self.0.arg {
                    write!(f, "in {arg}: ")?;
                }
                f.write_str(reason)
            }
        }
    }
}

impl Error {
    /// Create an custom error with given reason.
    pub fn custom(reason: impl Into<String>) -> Self {
        ErrorKind::Custom(reason.into()).into()
    }

    fn with_arg(mut self, arg: impl Into<OsString>) -> Self {
        self.0.arg = Some(Arg(arg.into()));
        self
    }

    #[cfg(feature = "help")]
    pub(crate) fn in_subcommand<C: CommandInternal>(mut self, name: String) -> Self {
        self.0.subcommand_path.push((name, C::COMMAND_INFO));
        self
    }

    #[cfg(not(feature = "help"))]
    pub(crate) fn in_subcommand<C: CommandInternal>(self, _name: String) -> Self {
        self
    }
}

impl From<ErrorKind> for Error {
    fn from(repr: ErrorKind) -> Self {
        Self(Box::new(Inner {
            arg: None,
            kind: repr,
            #[cfg(feature = "help")]
            subcommand_path: Vec::new(),
        }))
    }
}

impl ErrorKind {
    pub(crate) fn with_arg(self, arg: impl Into<OsString>) -> Error {
        Error::from(self).with_arg(arg)
    }
}
