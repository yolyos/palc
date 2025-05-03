use std::borrow::Cow;
use std::ffi::{OsStr, OsString};
use std::marker::PhantomData;
use std::num::NonZero;
use std::ops::ControlFlow;

use os_str_bytes::OsStrBytesExt;
use ref_cast::RefCast;

use crate::error::ErrorKind;
use crate::refl::{ArgsInfo, CommandInfo};
use crate::values::ArgValueInfo;
use crate::{Args, Result, Subcommand};

use super::Error;

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

pub fn place_for_subcommand<S: ParserState, const CUR_HAS_GLOBAL: bool>(
    state: &mut S,
) -> FeedUnnamed<'_> {
    #[derive(RefCast)]
    #[repr(transparent)]
    struct Place<S, const CUR_HAS_GLOBAL: bool>(S);

    impl<S: ParserState, const CUR_HAS_GLOBAL: bool> GreedyArgsPlace for Place<S, CUR_HAS_GLOBAL> {
        fn feed_greedy(
            &mut self,
            name: OsString,
            args: &mut ArgsIter<'_>,
            global: GlobalAncestors<'_>,
        ) -> Result<()> {
            let name = name.to_str().ok_or_else(|| ErrorKind::InvalidUtf8(name.clone()))?;
            let mut global = (&mut self.0, global);
            let global =
                if CUR_HAS_GLOBAL { &mut global as &mut dyn GlobalChain } else { global.1 };
            let subcmd = S::Subcommand::try_parse_with_name(name, args, global)
                .map_err(|err| err.in_subcommand::<S::Subcommand>(name.to_owned()))?;
            *S::subcommand_getter()(&mut self.0) = Some(subcmd);
            Ok(())
        }
    }

    Ok(Some(Place::<S, CUR_HAS_GLOBAL>::ref_cast_mut(state)))
}

/// Break on a resolved place. Continue on unknown names.
/// So we can `?` in generated code of `command(flatten)`.
pub type FeedNamed<'s> = ControlFlow<&'s mut dyn ArgPlace>;

/// This should be an enum, but be this for `?` support, which is unstable to impl.
pub type FeedUnnamed<'s> = Result<Option<&'s mut dyn GreedyArgsPlace>, Option<Error>>;

pub trait ArgsInternal: Sized + 'static {
    type __State: ParserState<Output = Self>;
}

pub trait ParserState: ParserStateDyn {
    type Output;
    type Subcommand: Subcommand;
    const ARGS_INFO: ArgsInfo;
    const TOTAL_UNNAMED_ARG_CNT: usize;

    fn init() -> Self;
    fn finish(self) -> Result<Self::Output>;
    fn subcommand_getter() -> impl Fn(&mut Self) -> &mut Option<Self::Subcommand>;

    // The is only called via `GlobalChain::search_global_named` thus do not need to be in vtable.
    fn feed_global_named(&mut self, _name: &str) -> FeedNamed<'_> {
        ControlFlow::Continue(())
    }
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

pub trait CommandInternal: Sized {
    const COMMAND_INFO: CommandInfo;

    fn try_parse_with_name(
        name: &str,
        args: &mut ArgsIter<'_>,
        global: GlobalAncestors<'_>,
    ) -> Result<Self>;
}

/// A common program-name-agnostic command.
impl<A: Args> CommandInternal for A {
    const COMMAND_INFO: CommandInfo = CommandInfo::new_catchall(&A::__State::ARGS_INFO);

    fn try_parse_with_name(
        _name: &str,
        args: &mut ArgsIter<'_>,
        global: GlobalAncestors<'_>,
    ) -> Result<Self> {
        try_parse_args(args, global)
    }
}

pub fn try_parse_args<A: Args>(args: &mut ArgsIter<'_>, global: GlobalAncestors<'_>) -> Result<A> {
    let mut state = A::__State::init();
    try_parse_with_state(&mut state, args, global)?;
    state.finish()
}

pub fn try_parse_with_state(
    state: &mut dyn ParserStateDyn,
    args: &mut ArgsIter<'_>,
    global: GlobalAncestors<'_>,
) -> Result<()> {
    let mut idx = 0usize;
    while let Some(arg) = args.cache_next_arg()? {
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
                        Err(None) => return Err(ErrorKind::UnexpectedUnnamedArgument(arg).into()),
                    }
                }
            }
            Arg::EncodedNamed(enc_name) => {
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
                            return Err(ErrorKind::UnknownNamedArgument.with_arg(enc_name));
                        }
                    },
                };
                match place.num_values() {
                    NumValues::Zero => {
                        args.check_no_value()?;
                        place.feed_none()?;
                    }
                    NumValues::One { require_equals: false } => {
                        place.feed(args.take_value()?)?;
                    }
                    NumValues::One { require_equals: true } => {
                        place.feed(Cow::Borrowed(args.take_value_after_eq()?))?;
                    }
                }
            }
            Arg::Unnamed(mut arg) => match state.feed_unnamed(&mut arg, idx, false) {
                Ok(None) => idx += 1,
                Ok(Some(place)) => return place.feed_greedy(arg, args, global),
                Err(Some(err)) => return Err(err),
                Err(None) => return Err(ErrorKind::UnexpectedUnnamedArgument(arg).into()),
            },
        }
    }
    Ok(())
}

pub struct ArgsIter<'a> {
    iter: &'a mut dyn Iterator<Item = OsString>,
    cur_input_arg: OsString,
    state: ArgsState,
}

#[derive(Debug)]
enum ArgsState {
    Unnamed,
    Long { eq_pos: Option<NonZero<usize>> },
    Short { next_pos: usize },
}

#[derive(Debug)]
enum Arg<'a> {
    DashDash,
    EncodedNamed(&'a str),
    Unnamed(OsString),
}

impl<'a> ArgsIter<'a> {
    pub(crate) fn new(iter: &'a mut dyn Iterator<Item = OsString>) -> Self {
        Self { iter, cur_input_arg: OsString::new(), state: ArgsState::Unnamed }
    }

    fn check_no_value(&mut self) -> Result<()> {
        // This only fail for long arguments with joined value: `--long=[..]`.
        if let ArgsState::Long { eq_pos: Some(pos) } = self.state {
            Err(ErrorKind::UnexpectedValue(
                self.cur_input_arg.index(pos.get() + 1..).to_os_string(),
            )
            .with_arg(self.cur_input_arg.index(..pos.get())))
        } else {
            Ok(())
        }
    }

    /// Retrieve the value for the current named argument after a mandatory "=".
    fn take_value_after_eq(&mut self) -> Result<&'_ OsStr> {
        match self.state {
            ArgsState::Long { eq_pos: Some(pos) } => Ok(self.cur_input_arg.index(pos.get() + 1..)),
            ArgsState::Short { next_pos }
                if self.cur_input_arg.as_encoded_bytes().get(next_pos) == Some(&b'=') =>
            {
                // Don't traverse the rest.
                self.state = ArgsState::Unnamed;
                Ok(self.cur_input_arg.index(next_pos + 1..))
            }
            // FIXME: Report error argument name.
            _ => Err(ErrorKind::MissingEq.into()),
        }
    }

    /// Retrieve the value for the current named argument, either after "=" or fetch from the next
    /// input argument.
    fn take_value(&mut self) -> Result<Cow<'_, OsStr>> {
        match self.state {
            ArgsState::Long { eq_pos: Some(pos) } => {
                Ok(Cow::Borrowed(self.cur_input_arg.index(pos.get() + 1..)))
            }
            ArgsState::Short { next_pos } if next_pos < self.cur_input_arg.len() => {
                let pos = if self.cur_input_arg.as_encoded_bytes()[next_pos] == b'=' {
                    next_pos + 1
                } else {
                    next_pos
                };
                // Don't traverse the rest.
                self.state = ArgsState::Unnamed;
                Ok(Cow::Borrowed(self.cur_input_arg.index(pos..)))
            }
            _ => {
                let Some(arg) = self.iter.next() else {
                    let name = std::mem::take(&mut self.cur_input_arg);
                    return Err(ErrorKind::MissingValue.with_arg(name));
                };
                Ok(Cow::Owned(arg))
            }
        }
    }

    /// Cache the next logical argument (short or long).
    fn cache_next_arg(&mut self) -> Result<Option<Arg<'_>>> {
        // Iterate the next short argument if we are inside a group of it.
        match self.state {
            ArgsState::Short { next_pos } if next_pos != self.cur_input_arg.len() => {
                let rest = &self.cur_input_arg.as_encoded_bytes()[next_pos..];
                // FIXME: More efficient way to get the next UTF-8 char?
                // UTF-8 char has encoded length 1..=4
                for len in 1..4 {
                    match std::str::from_utf8(&rest[..len]) {
                        Ok(s) => {
                            // Invariant: `prev_end..prev_end+len` is checked to be valid UTF-8.
                            self.state = ArgsState::Short { next_pos: next_pos + len };
                            return Ok(Some(Arg::EncodedNamed(s)));
                        }
                        // Incomplete encoding.
                        Err(e) if e.error_len().is_none() => {}
                        Err(_) => break,
                    }
                }
                return Err(ErrorKind::InvalidUtf8(
                    self.cur_input_arg.index(next_pos..).to_os_string(),
                )
                .into());
            }
            _ => {}
        }

        // Otherwise, fetch the next input argument.
        let Some(s) = self.iter.next() else {
            return Ok(None);
        };
        self.cur_input_arg = s;
        let argb = self.cur_input_arg.as_encoded_bytes();
        self.state = ArgsState::Unnamed;

        Ok(Some(if argb == b"-" {
            Arg::Unnamed(std::mem::take(&mut self.cur_input_arg))
        } else if argb == b"--" {
            Arg::DashDash
        } else if argb.starts_with(b"--") {
            let end = if let Some(pos) = argb.iter().position(|&b| b == b'=') {
                self.state = ArgsState::Long { eq_pos: NonZero::new(pos) };
                pos
            } else {
                self.state = ArgsState::Long { eq_pos: None };
                argb.len()
            };
            // Include preceeding "--" only for single-char long arguments.
            let start = if end == 3 { 0 } else { 2 };
            let s = self.cur_input_arg.index(start..end);
            Arg::EncodedNamed(s.to_str().ok_or_else(|| ErrorKind::InvalidUtf8(s.to_os_string()))?)
        } else if argb.starts_with(b"-") {
            self.state = ArgsState::Short { next_pos: 1 };
            return self.cache_next_arg();
        } else {
            Arg::Unnamed(std::mem::take(&mut self.cur_input_arg))
        }))
    }
}
