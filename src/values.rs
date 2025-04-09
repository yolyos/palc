use std::borrow::Cow;
use std::ffi::{OsStr, OsString};
use std::fmt;
use std::marker::PhantomData;
use std::ops::Deref;
use std::str::FromStr;

use crate::{ErrorKind, Result};

pub struct ArgValueInfo<P> {
    pub parser: P, // impl Fn(Cow<'_, OsStr>) -> Result<T>
}

pub trait ArgValue: Sized + 'static {
    fn parse_value(value: Cow<'_, OsStr>) -> Result<Self>;
}

#[macro_export]
#[doc(hidden)]
macro_rules! arg_value_info {
    ($ty:ty) => {
        ($crate::__private::InferValueParser::<$ty, &&&&()>($crate::__private::PhantomData).get())
    };
}

pub struct InferValueParser<T, Fuel>(pub PhantomData<(T, Fuel)>);

impl<T, Fuel> Deref for InferValueParser<T, &Fuel> {
    type Target = InferValueParser<T, Fuel>;
    fn deref(&self) -> &Self::Target {
        &InferValueParser(PhantomData)
    }
}

impl<T: ArgValue> InferValueParser<T, &&&&()> {
    pub fn get(&self) -> ArgValueInfo<impl Fn(Cow<'_, OsStr>) -> Result<T>> {
        ArgValueInfo {
            parser: T::parse_value,
        }
    }
}

impl<T: From<OsString>> InferValueParser<T, &&&()> {
    pub fn get(&self) -> ArgValueInfo<impl Fn(Cow<'_, OsStr>) -> Result<T>> {
        ArgValueInfo {
            parser: |v: Cow<'_, OsStr>| Ok(v.into_owned().into()),
        }
    }
}

impl<T: From<String>> InferValueParser<T, &&()> {
    pub fn get(&self) -> ArgValueInfo<impl Fn(Cow<'_, OsStr>) -> Result<T>> {
        ArgValueInfo {
            parser: |v: Cow<'_, OsStr>| {
                let s = v
                    .into_owned()
                    .into_string()
                    .map_err(ErrorKind::InvalidUtf8)?;
                Ok(s.into())
            },
        }
    }
}

impl<T: FromStr<Err: fmt::Display>> InferValueParser<T, &()> {
    pub fn get(&self) -> ArgValueInfo<impl Fn(Cow<'_, OsStr>) -> Result<T>> {
        ArgValueInfo {
            parser: |v: Cow<'_, OsStr>| {
                let s = v
                    .to_str()
                    .ok_or_else(|| ErrorKind::InvalidUtf8(v.to_os_string()))?;
                let v = s
                    .parse::<T>()
                    .map_err(|err| ErrorKind::InvalidValue(s.into(), err.to_string()))?;
                Ok(v)
            },
        }
    }
}

/// A placeholder struct to emit errors if a type is cannot be parsed.
#[expect(non_camel_case_types, reason = "for error display")]
pub struct Error__ThisTypeIsNotParseable<T>(PhantomData<T>);

impl<T> InferValueParser<T, ()> {
    pub fn get(&self) -> Error__ThisTypeIsNotParseable<T> {
        Error__ThisTypeIsNotParseable(PhantomData)
    }
}

#[test]
fn native_impls() {
    let _f = arg_value_info!(OsString).parser;
    let _f = arg_value_info!(std::path::PathBuf).parser;
    let _f = arg_value_info!(String).parser;
    let _f = arg_value_info!(usize).parser;
}

/// ```compile_fail
/// let _f = clap_static::arg_value_info!(()).parser;
/// ```
fn _not_implemented() {}
