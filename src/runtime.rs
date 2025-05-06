use std::borrow::Cow;
use std::convert::Infallible;
use std::ffi::{OsStr, OsString};
use std::marker::PhantomData;
use std::num::NonZero;
use std::ops::ControlFlow;

use os_str_bytes::OsStrBytesExt;
use ref_cast::RefCast;

use crate::error::ErrorKind;
use crate::refl::{RawArgsInfo, RawCommandInfo};
use crate::values::ArgValueInfo;
use crate::{Args, Result, Subcommand};

use super::Error;

// The fake sealed trait to show in docs.
// Proc-macro can still generate its impl.
pub trait Sealed {}

#[macro_export]
#[doc(hidden)]
macro_rules! __const_concat {
    // Fast path for default `__raw_meta`.
    ($($s:literal,)* $(env!($e:literal), $($s2:literal,)*)*) => {
        $crate::__private::concat!($($s,)* $(env!($e), $($s2,)*)*)
    };

    // Match exprs after literals to prevent invisible grouping.
    ($s:expr,) => {
        $s
    };
    ($($s:expr,)*) => {{
        const __STRS: &'static [&'static $crate::__private::str] = &[$($s),*];
        match $crate::__private::from_utf8(&const {
            $crate::__private::const_concat_impl::<{
                $crate::__private::const_concat_len(__STRS)
            }>(__STRS)
        }) {
            $crate::__private::Ok(__s) => __s,
            $crate::__private::Err(_) => $crate::__private::unreachable!(),
        }
    }};
}

pub const fn const_concat_len(strs: &[&str]) -> usize {
    let mut ret = 0;
    let mut i = 0;
    let str_cnt = strs.len();
    while i < str_cnt {
        ret += strs[i].len();
        i += 1;
    }
    ret
}

pub const fn const_concat_impl<const LEN: usize>(strs: &[&str]) -> [u8; LEN] {
    // Invalid UTF-8, to assert `LEN` is not too long.
    let mut buf = [0xFFu8; LEN];
    let mut o = 0;
    let mut i = 0;
    let str_cnt = strs.len();
    while i < str_cnt {
        let s = strs[i].as_bytes();
        let mut j = 0;
        let str_len = s.len();
        while j < str_len {
            buf[o] = s[j];
            j += 1;
            o += 1;
        }
        i += 1;
    }
    buf
}

/// The fallback state type for graceful failing from proc-macro.
pub struct FallbackState<T>(Infallible, PhantomData<T>);

impl<T: 'static> ParserState for FallbackState<T> {
    type Output = T;

    const RAW_ARGS_INFO: RawArgsInfo = RawArgsInfo::empty();
    const TOTAL_UNNAMED_ARG_CNT: usize = 0;

    fn init() -> Self {
        unimplemented!()
    }
    fn finish(self) -> Result<Self::Output> {
        match self {}
    }
}
impl<T: 'static> ParserStateDyn for FallbackState<T> {}

// TODO: Check inlining behavior is expected.
pub fn unknown_subcommand<T>(name: &OsStr) -> Result<T> {
    Err(ErrorKind::UnknownSubcommand.with_input(name.into()))
}

pub fn missing_required_arg<T>(enc_arg: &'static str) -> Result<T> {
    Err(ErrorKind::MissingRequiredArgument.with_arg(enc_arg))
}

pub fn missing_required_subcmd<T>() -> Result<T> {
    Err(ErrorKind::MissingRequiredSubcommand.into())
}

// TODO: Detail errors.
pub fn fail_constraint<T>(arg: &'static str) -> Result<T> {
    Err(ErrorKind::Constraint.with_arg(arg))
}

#[inline]
pub fn take_arg(s: &mut OsString) -> Cow<'static, OsStr> {
    Cow::Owned(std::mem::take(s))
}

/// A named argument with its place attached as `&mut self`.
pub trait ArgPlace {
    fn num_values(&self) -> NumValues;

    // FIXME: Merge these functions?
    fn feed(&mut self, _value: Cow<'_, OsStr>) -> Result<(), Error> {
        unreachable!()
    }
    fn feed_none(&mut self) -> Result<(), Error> {
        unreachable!()
    }
}

/// The expected number of values for a named argument to take.
#[derive(Debug, Clone, Copy)]
pub enum NumValues {
    Zero,
    One { require_equals: bool },
}

#[inline(always)]
pub fn place_for_flag(place: &mut Option<bool>) -> &mut dyn ArgPlace {
    #[derive(RefCast)]
    #[repr(transparent)]
    struct Place(Option<bool>);

    impl ArgPlace for Place {
        fn num_values(&self) -> NumValues {
            NumValues::Zero
        }
        fn feed_none(&mut self) -> Result<(), Error> {
            self.0 = Some(true);
            Ok(())
        }
    }

    Place::ref_cast_mut(place)
}

#[inline(always)]
pub fn place_for_counter(place: &mut Option<u8>) -> &mut dyn ArgPlace {
    #[derive(RefCast)]
    #[repr(transparent)]
    struct Place(Option<u8>);

    impl ArgPlace for Place {
        fn num_values(&self) -> NumValues {
            NumValues::Zero
        }
        fn feed_none(&mut self) -> Result<(), Error> {
            let v = self.0.get_or_insert_default();
            *v = v.saturating_add(1);
            Ok(())
        }
    }

    Place::ref_cast_mut(place)
}

pub fn place_for_vec<T, A: ArgValueInfo<T>, const REQUIRE_EQ: bool>(
    place: &mut Option<Vec<T>>,
    _: A,
) -> &mut dyn ArgPlace {
    #[derive(RefCast)]
    #[repr(transparent)]
    struct Place<T, A, const REQUIRE_EQ: bool>(Option<Vec<T>>, PhantomData<A>);

    impl<T, A: ArgValueInfo<T>, const REQUIRE_EQ: bool> ArgPlace for Place<T, A, REQUIRE_EQ> {
        fn num_values(&self) -> NumValues {
            NumValues::One { require_equals: REQUIRE_EQ }
        }

        fn feed(&mut self, value: Cow<'_, OsStr>) -> Result<(), Error> {
            self.0.get_or_insert_default().push(A::parser()(value)?);
            Ok(())
        }
    }

    Place::<T, A, REQUIRE_EQ>::ref_cast_mut(place)
}

pub fn place_for_vec_sep<T, A: ArgValueInfo<T>, const REQUIRE_EQ: bool, const DELIMITER: char>(
    place: &mut Option<Vec<T>>,
    _: A,
) -> &'_ mut dyn ArgPlace {
    #[derive(RefCast)]
    #[repr(transparent)]
    struct Place<T, A, const REQUIRE_EQ: bool, const DELIMITER: char>(
        Option<Vec<T>>,
        PhantomData<A>,
    );

    impl<T, A: ArgValueInfo<T>, const REQUIRE_EQ: bool, const DELIMITER: char> ArgPlace
        for Place<T, A, REQUIRE_EQ, DELIMITER>
    {
        fn num_values(&self) -> NumValues {
            NumValues::One { require_equals: REQUIRE_EQ }
        }

        fn feed(&mut self, value: Cow<'_, OsStr>) -> Result<(), Error> {
            let parser = A::parser();
            let v = self.0.get_or_insert_default();
            for frag in value.split(DELIMITER) {
                v.push(parser(Cow::Borrowed(frag))?);
            }
            Ok(())
        }
    }

    Place::<T, A, REQUIRE_EQ, DELIMITER>::ref_cast_mut(place)
}

pub fn place_for_set_value<T, A: ArgValueInfo<T>, const REQUIRE_EQ: bool>(
    place: &mut Option<T>,
    _: A,
) -> &'_ mut dyn ArgPlace {
    #[derive(RefCast)]
    #[repr(transparent)]
    struct Place<T, A, const REQUIRE_EQ: bool>(Option<T>, PhantomData<A>);

    impl<T, A: ArgValueInfo<T>, const REQUIRE_EQ: bool> ArgPlace for Place<T, A, REQUIRE_EQ> {
        fn num_values(&self) -> NumValues {
            NumValues::One { require_equals: REQUIRE_EQ }
        }

        fn feed(&mut self, value: Cow<'_, OsStr>) -> Result<(), Error> {
            self.0 = Some(A::parser()(value)?);
            Ok(())
        }
    }

    Place::<T, A, REQUIRE_EQ>::ref_cast_mut(place)
}

pub type GlobalAncestors<'a> = &'a mut dyn GlobalChain;

/// The singly linked list for states of ancestor subcommands that accept any global arguments.
/// Deeper states come first.
/// It behaves like:
/// `struct Node<'_> { node: &'_ mut dyn ParserStateDyn, parent: Option<&'_ mut Node<'_>> }`
/// except lifetimes do not work without type erasure (`dyn`).
pub trait GlobalChain {
    fn search_global_named(&mut self, _enc_name: &str) -> FeedNamed<'_> {
        ControlFlow::Continue(())
    }
}

impl GlobalChain for () {}
impl<S: ParserState> GlobalChain for (&mut S, &mut dyn GlobalChain) {
    fn search_global_named(&mut self, enc_name: &str) -> FeedNamed<'_> {
        self.0.feed_global_named(enc_name)?;
        self.1.search_global_named(enc_name)
    }
}

/// A greedy unnamed argument with its place attached as `&mut self`.
///
/// It always consumes all the rest arguments.
pub trait GreedyArgsPlace {
    // TODO: Maybe avoid splitting out the first argument?
    fn feed_greedy(
        &mut self,
        arg: OsString,
        args: &mut ArgsIter<'_>,
        global: GlobalAncestors<'_>,
    ) -> Result<()>;
}

pub fn place_for_trailing_var_arg<T, A: ArgValueInfo<T>>(
    place: &mut Option<Vec<T>>,
    _: A,
) -> FeedUnnamed<'_> {
    #[derive(RefCast)]
    #[repr(transparent)]
    struct Place<T, A>(Option<Vec<T>>, PhantomData<A>);

    impl<T, A: ArgValueInfo<T>> GreedyArgsPlace for Place<T, A> {
        fn feed_greedy(
            &mut self,
            mut arg: OsString,
            args: &mut ArgsIter<'_>,
            _global: GlobalAncestors<'_>,
        ) -> Result<()> {
            let v = self.0.get_or_insert_default();
            if let Some(high) = args.iter.size_hint().1 {
                v.reserve(1 + high);
            }
            let parser = A::parser();
            loop {
                v.push(parser(Cow::Owned(arg))?);
                arg = match args.iter.next() {
                    Some(arg) => arg,
                    None => return Ok(()),
                };
            }
        }
    }

    Ok(Some(Place::<T, A>::ref_cast_mut(place)))
}

pub fn place_for_subcommand<G: GetSubcommand, const CUR_HAS_GLOBAL: bool>(
    state: &mut G::State,
) -> FeedUnnamed<'_> {
    #[derive(RefCast)]
    #[repr(transparent)]
    struct Place<G: GetSubcommand, const CUR_HAS_GLOBAL: bool>(G::State);

    impl<G: GetSubcommand, const CUR_HAS_GLOBAL: bool> GreedyArgsPlace for Place<G, CUR_HAS_GLOBAL> {
        fn feed_greedy(
            &mut self,
            name: OsString,
            args: &mut ArgsIter<'_>,
            global: GlobalAncestors<'_>,
        ) -> Result<()> {
            let mut global = (&mut self.0, global);
            let global =
                if CUR_HAS_GLOBAL { &mut global as &mut dyn GlobalChain } else { global.1 };
            let subcmd = G::Subcommand::try_parse_with_name(name, args, global)?;
            *G::get(&mut self.0) = Some(subcmd);
            Ok(())
        }
    }

    Ok(Some(Place::<G, CUR_HAS_GLOBAL>::ref_cast_mut(state)))
}

/// Break on a resolved place. Continue on unknown names.
/// So we can `?` in generated code of `command(flatten)`.
pub type FeedNamed<'s> = ControlFlow<&'s mut dyn ArgPlace>;

/// This should be an enum, but be this for `?` support, which is unstable to impl.
pub type FeedUnnamed<'s> = Result<Option<&'s mut dyn GreedyArgsPlace>, Option<Error>>;

pub trait ParserState: ParserStateDyn {
    type Output;

    // This is stored by-value, because we only want to promote it to
    // `&'static [RawArgsInfo]` after processing all `command(flatten)`.
    const RAW_ARGS_INFO: RawArgsInfo;
    const TOTAL_UNNAMED_ARG_CNT: usize;

    fn init() -> Self;
    fn finish(self) -> Result<Self::Output>;

    // The is only called via `GlobalChain::search_global_named` thus do not need to be in vtable.
    fn feed_global_named(&mut self, _name: &str) -> FeedNamed<'_> {
        ControlFlow::Continue(())
    }
}

/// The helper trait for `place_for_subcommand`.
///
/// It is possible to merge these methods into `ParserState`, but that would
/// expose `Subcommand` type at `UserParser::__State::Subcommand`, causing
/// various privacy issues if `UserParser` and its subcommand have different
/// privacy.
///
/// Here we define a separated (public) trait, but let proc-macro generate a
/// private witness type inside `feed_unnamed`, hiding the subcommand type from
/// public interface.
pub trait GetSubcommand: 'static {
    type State: ParserState;
    type Subcommand: Subcommand;
    fn get(state: &mut Self::State) -> &mut Option<Self::Subcommand>;
}

pub trait ParserStateDyn: 'static {
    /// Try to accept a named argument.
    ///
    /// If this parser accepts named argument `name`, return `Break(argument_place)`;
    /// otherwise, return `Continue(())`.
    ///
    /// `enc_name` is the encoded argument name to be matched on:
    /// - "-s" => "s"
    /// - "--long" => "long"
    /// - "--l" => "--l", to disambiguate from short arguments.
    fn feed_named(&mut self, _enc_name: &str) -> FeedNamed<'_> {
        ControlFlow::Continue(())
    }

    /// Try to accept an unnamed (positional) argument.
    ///
    /// `idx` is the index of logical arguments, counting each multi-value-argument as one.
    /// `is_last` indices if a `--` has been encountered. It does not effect `idx`.
    fn feed_unnamed(&mut self, arg: &mut OsString, idx: usize, is_last: bool) -> FeedUnnamed {
        let _ = (arg, idx, is_last);
        Err(None)
    }
}

/// No-op state for unit structs or unit variants.
///
/// We still need to fully consume the input for global arguments and error reporting.
impl ParserState for () {
    type Output = ();

    const RAW_ARGS_INFO: RawArgsInfo = RawArgsInfo::empty();
    const TOTAL_UNNAMED_ARG_CNT: usize = 0;

    fn init() -> Self {}

    fn finish(self) -> Result<Self::Output> {
        Ok(())
    }
}
impl ParserStateDyn for () {}
impl Args for () {
    type __State = ();
}
impl Sealed for () {}

/// The parser fn signature that matches [`try_parse_args`].
type ArgsParserFn<T> = fn(args: &mut ArgsIter<'_>, global: GlobalAncestors<'_>) -> Result<T>;

#[cfg(test)]
fn _assert_args_fn_sig<A: Args>() -> ArgsParserFn<A> {
    try_parse_args::<A>
}

pub type FeedSubcommand<T> = Option<ArgsParserFn<T>>;

pub trait CommandInternal: Sized {
    // This is stored as reference, since we always use it as a reference in
    // reflection structure. There is no benefit to inline it.
    const RAW_COMMAND_INFO: &'static RawCommandInfo;

    fn feed_subcommand(_name: &OsStr) -> FeedSubcommand<Self> {
        None
    }

    fn try_parse_with_name(
        name: OsString,
        args: &mut ArgsIter<'_>,
        global: GlobalAncestors<'_>,
    ) -> Result<Self> {
        let Some(f) = Self::feed_subcommand(&name) else {
            return unknown_subcommand(&name);
        };
        // There is a matching subcommand, thus `name` must be UTF-8.
        f(args, global).map_err(|err| err.in_subcommand::<Self>(name.into_string().unwrap()))
    }
}

pub fn try_parse_args<A: Args>(args: &mut ArgsIter<'_>, global: GlobalAncestors<'_>) -> Result<A> {
    try_parse_state::<A::__State>(args, global)
}

pub fn try_parse_state<S: ParserState>(
    args: &mut ArgsIter<'_>,
    global: GlobalAncestors<'_>,
) -> Result<S::Output> {
    let mut state = S::init();
    try_parse_with_state(&mut state, args, global)?;
    state.finish()
}

#[inline(never)]
pub fn try_parse_with_state(
    state: &mut dyn ParserStateDyn,
    args: &mut ArgsIter<'_>,
    global: GlobalAncestors<'_>,
) -> Result<()> {
    let mut idx = 0usize;
    let mut buf = OsString::new();
    while let Some(arg) = args.next_arg(&mut buf)? {
        match arg {
            Arg::DashDash => {
                drop(arg);
                for mut arg in &mut args.iter {
                    match state.feed_unnamed(&mut arg, idx, true) {
                        Ok(None) => idx += 1,
                        Ok(Some(place)) => {
                            return place.feed_greedy(arg, args, global);
                        }
                        Err(Some(err)) => return Err(err),
                        Err(None) => return Err(ErrorKind::ExtraUnnamedArgument.with_input(arg)),
                    }
                }
                return Ok(());
            }
            Arg::EncodedNamed(enc_name, has_eq, value) => {
                let place = match state.feed_named(enc_name) {
                    ControlFlow::Break(place) => place,
                    ControlFlow::Continue(()) => match global.search_global_named(enc_name) {
                        ControlFlow::Break(place) => place,
                        ControlFlow::Continue(()) => {
                            // TODO: Configurable help?
                            #[cfg(feature = "help")]
                            if enc_name == "h" || enc_name == "help" {
                                return Err(ErrorKind::Help.into());
                            }
                            let mut dec_name = String::with_capacity(2 + enc_name.len());
                            // TODO: Dedup this code with `Error::fmt`.
                            if enc_name.chars().nth(1).is_none() {
                                dec_name.push('-');
                            } else if !enc_name.starts_with("--") {
                                dec_name.push_str("--");
                            }
                            dec_name.push_str(enc_name);
                            return Err(ErrorKind::UnknownNamedArgument.with_input(dec_name.into()));
                        }
                    },
                };

                match place.num_values() {
                    NumValues::Zero => {
                        // Only fail on long arguments with inlined values `--long=value`.
                        if enc_name.len() > 1 {
                            if let Some(v) = value {
                                return Err(ErrorKind::UnexpectedInlineValue
                                    .with_arg_input(enc_name, v.into()));
                            }
                        }
                        place.feed_none()?;
                    }
                    NumValues::One { require_equals } => {
                        if require_equals && !has_eq {
                            return Err(ErrorKind::MissingEq.with_arg(enc_name));
                        }
                        if let Some(v) = value {
                            place.feed(Cow::Borrowed(v))?;
                            args.discard_short_args();
                        } else {
                            let v = args.next_value(enc_name)?;
                            place.feed(Cow::Owned(v))?;
                        }
                    }
                }
            }
            Arg::Unnamed(mut arg) => match state.feed_unnamed(&mut arg, idx, false) {
                Ok(None) => idx += 1,
                Ok(Some(place)) => return place.feed_greedy(arg, args, global),
                Err(Some(err)) => return Err(err),
                Err(None) => return Err(ErrorKind::ExtraUnnamedArgument.with_input(arg)),
            },
        }
    }
    Ok(())
}

pub struct ArgsIter<'a> {
    iter: &'a mut dyn Iterator<Item = OsString>,
    /// If we are inside a short arguments bundle, the index of next short arg.
    next_short_idx: Option<NonZero<usize>>,
}

#[derive(Debug)]
enum Arg<'a> {
    /// "--"
    DashDash,
    /// Encoded arg name, equal sign, and an inlined value (excluding `=`).
    ///
    /// - "--long" => ("long", None)
    /// - "--long=value" => ("long", Some("value"))
    /// - "-s" => ("s", None)
    /// - "-smore", "-s=more" => ("s", Some("more"))
    EncodedNamed(&'a str, bool, Option<&'a OsStr>),
    Unnamed(OsString),
}

impl<'a> ArgsIter<'a> {
    pub(crate) fn new(iter: &'a mut dyn Iterator<Item = OsString>) -> Self {
        Self { iter, next_short_idx: None }
    }

    /// Iterate the next logical argument, possibly splitting short argument bundle.
    fn next_arg<'b>(&mut self, buf: &'b mut OsString) -> Result<Option<Arg<'b>>> {
        #[cold]
        fn fail_on_next_short_arg(rest: &OsStr) -> Error {
            let bytes = rest.as_encoded_bytes();

            // UTF-8 length of a char must be 1..=4, len==1 case is checked outside.
            for len in 2..=bytes.len().min(4) {
                if let Ok(s) = std::str::from_utf8(&bytes[..len]) {
                    let mut dec_input = String::with_capacity(4);
                    dec_input.push('-');
                    dec_input.push_str(s);
                    return ErrorKind::UnknownNamedArgument.with_input(dec_input.into());
                }
            }
            ErrorKind::InvalidUtf8.with_input(rest.into())
        }

        if let Some(pos) = self.next_short_idx.filter(|pos| pos.get() < buf.len()) {
            let argb = buf.as_encoded_bytes();
            let idx = pos.get();

            // By struct invariant, argb[..idx] must be UTF-8.
            let next_byte = std::str::from_utf8(&argb[idx..idx + 1]);
            // Assuming all valid short args are ASCII, if the next byte is not ASCII, it must fail.
            let short_arg = next_byte.map_err(|_| fail_on_next_short_arg(buf.index(idx..)))?;

            self.next_short_idx = pos.checked_add(1);
            let (has_eq, value) = match argb.get(idx + 1) {
                Some(&b'=') => (true, Some(buf.index(idx + 2..))),
                Some(_) => (false, Some(buf.index(idx + 1..))),
                None => {
                    // Reached the end of bundle.
                    self.discard_short_args();
                    (false, None)
                }
            };
            return Ok(Some(Arg::EncodedNamed(short_arg, has_eq, value)));
        }
        self.next_short_idx = None;

        // Otherwise, fetch the next input argument.
        *buf = match self.iter.next() {
            Some(raw) => raw,
            None => return Ok(None),
        };

        if buf.starts_with("--") {
            if buf.len() == 2 {
                return Ok(Some(Arg::DashDash));
            }
            // Using `strip_prefix` in if-condition requires polonius to make lifetime check.
            let rest = buf.index(2..);
            let (name, has_eq, value) = match rest.split_once('=') {
                Some((name, value)) => (name, true, Some(value)),
                None => (rest, false, None),
            };
            // Include proceeding "--" only for single-char long arguments.
            let enc_name = if name.len() != 1 { name } else { buf.index(..3) };
            let enc_name = enc_name
                .to_str()
                .ok_or_else(|| ErrorKind::InvalidUtf8.with_input(enc_name.into()))?;
            Ok(Some(Arg::EncodedNamed(enc_name, has_eq, value)))
        } else if buf.starts_with("-") && buf.len() != 1 {
            self.next_short_idx = Some(NonZero::new(1).unwrap());
            return self.next_arg(buf);
        } else {
            Ok(Some(Arg::Unnamed(std::mem::take(buf))))
        }
    }

    /// Discard the rest of short argument bundle, poll a new raw argument on next `next_arg`.
    fn discard_short_args(&mut self) {
        self.next_short_idx = None;
    }

    fn next_value(&mut self, enc_name: &str) -> Result<OsString> {
        assert!(self.next_short_idx.is_none());
        self.iter
            .next()
            // TODO: allow_hyphen.
            .filter(|raw| raw == "-" || !raw.starts_with('='))
            .ok_or_else(|| ErrorKind::MissingValue.with_arg(enc_name))
    }
}
