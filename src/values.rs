use std::borrow::Cow;
use std::ffi::{OsStr, OsString};
use std::fmt;
use std::marker::PhantomData;
use std::ops::Deref;
use std::str::FromStr;

use crate::{ErrorKind, Result};

mod sealed {
    pub trait Sealed {}
}

#[diagnostic::on_unimplemented(
    message = "`{Self}` cannot be parsed into a clap_static argument value",
    label = "Unparseable type",
    note = "for enum types, try `derive(clap_static::ValueEnum)` on it",
    note = "or you can implement either `From<OsString>`, `From<String>` or `FromStr` for it \
    to make it parseable"
)]
pub trait ArgValueInfo<T>: 'static + Sized + sealed::Sealed {
    fn parser() -> impl Fn(Cow<'_, OsStr>) -> Result<T>;

    #[inline]
    fn parse(self, v: Cow<'_, OsStr>) -> Result<T> {
        Self::parser()(v)
    }

    #[inline]
    fn parse_str(self, v: &str) -> Result<T> {
        Self::parse(self, Cow::Borrowed(OsStr::new(v)))
    }
}

pub trait ValueEnum: Sized {
    fn parse_value(s: &str) -> Option<Self>;
}

#[macro_export]
#[doc(hidden)]
macro_rules! arg_value_info {
    ($ty:ty) => {
        $crate::__private::InferValueParser::<$ty, &&&&()>($crate::__private::PhantomData).get()
    };
}

pub struct InferValueParser<T, Fuel>(pub PhantomData<(T, Fuel)>);

impl<T, Fuel> Deref for InferValueParser<T, &Fuel> {
    type Target = InferValueParser<T, Fuel>;
    fn deref(&self) -> &Self::Target {
        &InferValueParser(PhantomData)
    }
}

impl<T: ValueEnum> InferValueParser<T, &&&&()> {
    pub fn get(&self) -> impl ArgValueInfo<T> {
        struct Info;
        impl sealed::Sealed for Info {}
        impl<T: ValueEnum> ArgValueInfo<T> for Info {
            fn parser() -> impl Fn(Cow<'_, OsStr>) -> Result<T> {
                |v| {
                    let s = v
                        .to_str()
                        .ok_or_else(|| ErrorKind::InvalidUtf8(v.to_os_string()))?;
                    // TODO: better diagnostics?
                    Ok(T::parse_value(s).ok_or_else(|| {
                        ErrorKind::InvalidValue(s.into(), "unknown variant".into())
                    })?)
                }
            }
        }
        Info
    }
}

impl<T: From<OsString>> InferValueParser<T, &&&()> {
    pub fn get(&self) -> impl ArgValueInfo<T> {
        struct Info;
        impl sealed::Sealed for Info {}
        impl<T: From<OsString>> ArgValueInfo<T> for Info {
            fn parser() -> impl Fn(Cow<'_, OsStr>) -> Result<T> {
                |v| Ok(v.into_owned().into())
            }
        }
        Info
    }
}

impl<T: From<String>> InferValueParser<T, &&()> {
    pub fn get(&self) -> impl ArgValueInfo<T> {
        struct Info;
        impl sealed::Sealed for Info {}
        impl<T: From<String>> ArgValueInfo<T> for Info {
            fn parser() -> impl Fn(Cow<'_, OsStr>) -> Result<T> {
                |v| {
                    Ok(v.into_owned()
                        .into_string()
                        .map_err(ErrorKind::InvalidUtf8)?
                        .into())
                }
            }
        }
        Info
    }
}

impl<T: FromStr<Err: fmt::Display>> InferValueParser<T, &()> {
    pub fn get(&self) -> impl ArgValueInfo<T> {
        struct Info;
        impl sealed::Sealed for Info {}
        impl<T: FromStr<Err: fmt::Display>> ArgValueInfo<T> for Info {
            fn parser() -> impl Fn(Cow<'_, OsStr>) -> Result<T> {
                |v| {
                    Ok(v.to_str()
                        .ok_or_else(|| ErrorKind::InvalidUtf8(v.to_os_string()))?
                        .parse::<T>()
                        .map_err(|err| ErrorKind::InvalidValue(v.into(), err.to_string()))?)
                }
            }
        }
        Info
    }
}

// For error reporting.
// Since `ArgValueInfo` is sealed and all implementations are private, this user type is guaranteed
// to cause an unimplemented error on `ArgValueInfo`.
impl<T> InferValueParser<T, ()> {
    pub fn get(&self) -> T {
        unreachable!()
    }
}

#[test]
fn native_impls() {
    use std::path::PathBuf;

    fn has_parser<T>(_: impl ArgValueInfo<T>) {}

    has_parser::<OsString>(arg_value_info!(OsString));
    has_parser::<PathBuf>(arg_value_info!(PathBuf));
    has_parser::<String>(arg_value_info!(String));
    has_parser::<usize>(arg_value_info!(usize));

    let _ = || {
        let () = arg_value_info!(());
    };
}
