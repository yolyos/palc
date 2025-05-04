// TODO: Better Debug impl for structs in this mod.
fn opt(s: &str) -> Option<&str> {
    if s.is_empty() { None } else { Some(s) }
}

/// Description of a collection of arguments.
///
/// NB. This struct is constructed by proc-macro.
#[doc(hidden)]
#[derive(Debug)]
pub struct RawArgsInfo {
    /// Subcommand argument, if any.
    pub __subcommand: Option<&'static RawCommandInfo>,

    /// Zero or more '\0'-terminated raw `ArgInfo`.
    ///
    /// Each raw `ArgInfo` consists of '\n'-terminated elements in following order:
    /// - Short names concatenated without separator.
    /// - Long names concatenated, each terminated by '\t'.
    /// - Value names concatenated, each terminated by '\t'.
    /// - `require_eq as u8`
    /// - `greedy as u8`
    /// - Help text.
    ///
    /// If either short or long names is non-empty, this is a named argument,
    /// otherwise it is an unnamed one.
    pub __raw_args: &'static str,

    /// The first byte is '1' if there is an optional subcommand, otherwise '0'.
    ///
    /// If command doc is enabled, there are additionally NUL-separated elements
    /// in following order:
    /// - `name`
    /// - `version`
    /// - `author`
    /// - `about`
    /// - `long_about`
    /// - `long_help`
    /// - `after_long_help`
    pub __raw_meta: &'static str,
}

impl RawArgsInfo {
    #[doc(hidden)]
    pub const fn empty() -> Self {
        Self { __subcommand: None, __raw_args: "", __raw_meta: "0" }
    }
}

/// Description of a collection of arguments.
#[derive(Debug, Clone, Copy)]
pub struct ArgsInfo {
    raw_args: &'static str,
    subcommand: Option<&'static RawCommandInfo>,
    is_subcommand_optional: bool,
    raw_doc: &'static str,
}

impl ArgsInfo {
    fn from_raw(raw: &'static RawArgsInfo) -> Self {
        // See `RawArgsInfo`.
        let (fst, raw_doc) = raw.__raw_meta.split_at(1);
        Self {
            raw_args: raw.__raw_args,
            subcommand: raw.__subcommand,
            is_subcommand_optional: fst == "1",
            raw_doc,
        }
    }

    pub fn args(&self) -> impl Iterator<Item = ArgInfo> {
        // See `RawArgsInfo`.
        self.raw_args.split_terminator('\0').map(ArgInfo::from_raw)
    }

    pub fn named_args(&self) -> impl Iterator<Item = NamedArgInfo> {
        self.args().filter_map(ArgInfo::to_named)
    }

    pub fn unnamed_args(&self) -> impl Iterator<Item = UnnamedArgInfo> {
        self.args().filter_map(ArgInfo::to_unnamed)
    }

    pub fn subcommand(&self) -> Option<SubcommandInfo> {
        self.subcommand.map(SubcommandInfo::from_raw)
    }

    pub fn is_subcommand_optional(&self) -> bool {
        self.is_subcommand_optional
    }

    pub fn doc(&self) -> Option<CommandDoc> {
        // See `RawArgsInfo`.
        let mut it = self.raw_doc.splitn(7, '\0');
        let name = it.next()?;
        let version = opt(it.next()?);
        let author = opt(it.next()?);
        let about = opt(it.next()?);
        let long_about = opt(it.next()?);
        let after_help = opt(it.next()?);
        let after_long_help = opt(it.next()?);
        Some(CommandDoc { name, version, author, about, long_about, after_help, after_long_help })
    }
}

/// Description of an arguments.
#[derive(Debug, Clone, Copy)]
pub enum ArgInfo {
    Named(NamedArgInfo),
    Unnamed(UnnamedArgInfo),
}

impl ArgInfo {
    // See `RawArgsInfo`.
    fn from_raw(raw: &'static str) -> Self {
        (|| {
            let mut it = raw.splitn(6, '\n');
            let raw_short_names = it.next()?;
            let raw_long_names = it.next()?;
            let raw_value_names = it.next()?;
            let require_eq = it.next()? == "1";
            let greedy = it.next()? == "1";
            let long_help = it.next()?;
            Some(if !raw_short_names.is_empty() || !raw_long_names.is_empty() {
                Self::Named(NamedArgInfo {
                    raw_short_names,
                    raw_long_names,
                    raw_value_names,
                    require_eq,
                    greedy,
                    long_help,
                })
            } else {
                Self::Unnamed(UnnamedArgInfo { raw_value_names, greedy, long_help })
            })
        })()
        .unwrap()
    }

    pub fn is_named(&self) -> bool {
        matches!(self, Self::Named(..))
    }

    pub fn to_named(self) -> Option<NamedArgInfo> {
        if let Self::Named(v) = self { Some(v) } else { None }
    }

    pub fn to_unnamed(self) -> Option<UnnamedArgInfo> {
        if let Self::Unnamed(v) = self { Some(v) } else { None }
    }
}

/// Description of a named argument.
#[derive(Debug, Clone, Copy)]
pub struct NamedArgInfo {
    raw_long_names: &'static str,
    raw_short_names: &'static str,
    raw_value_names: &'static str,
    require_eq: bool,
    greedy: bool,
    long_help: &'static str,
}

impl NamedArgInfo {
    pub fn short_names(&self) -> impl Iterator<Item = char> {
        self.raw_short_names.chars()
    }

    pub fn long_names(&self) -> impl Iterator<Item = &'static str> {
        // See `RawArgInfo`
        self.raw_long_names.split_terminator('\t').filter(|s| !s.is_empty())
    }

    pub fn value_names(&self) -> impl Iterator<Item = &'static str> {
        // See `RawArgInfo`
        self.raw_value_names.split_terminator('\t').filter(|s| !s.is_empty())
    }

    pub fn requires_eq(&self) -> bool {
        self.require_eq
    }

    pub fn greedy(&self) -> bool {
        self.greedy
    }

    pub fn long_help(&self) -> Option<&'static str> {
        opt(self.long_help)
    }
}

/// Description of an unnamed (positional) argument.
#[derive(Debug, Clone, Copy)]
pub struct UnnamedArgInfo {
    raw_value_names: &'static str,
    greedy: bool,
    long_help: &'static str,
}

impl UnnamedArgInfo {
    pub fn value_names(&self) -> impl Iterator<Item = &'static str> {
        // See `ArgInfo::from_raw`
        self.raw_value_names.split_terminator('\t')
    }

    pub fn greedy(&self) -> bool {
        self.greedy
    }

    pub fn long_help(&self) -> Option<&'static str> {
        opt(self.long_help)
    }
}

/// NB. This struct is constructed by proc-macro.
///
/// - For regular `derive(Subcommand)` enums, `__raw_names` has the same number
///   of names as `__subcommands`.
/// - For `derive(Parser)` structs, `__raw_names` is empty and `__subcommands`
///   has exactly one element. Note that in this case, this struct cannot be
///   used as a subcommand of other `derive(Args)` structs.
#[doc(hidden)]
#[derive(Debug, Clone, Copy)]
pub struct RawCommandInfo {
    /// '\t'-terminated subcommand names.
    pub __raw_names: &'static str,
    /// Argument of each subcommand.
    pub __subcommands: &'static [RawArgsInfo],
}

impl RawCommandInfo {
    pub(crate) const fn empty() -> Self {
        Self { __raw_names: "", __subcommands: &[] }
    }

    // pub(crate) fn find_subcommand(&self)
}

/// Description of a command applet.
#[derive(Debug, Clone, Copy)]
pub enum CommandInfo {
    /// A top-level program-name-agnostic `Parser` struct.
    RootArgs(ArgsInfo),
    Subcommand(SubcommandInfo),
}

impl CommandInfo {
    pub(crate) fn from_raw(raw: &'static RawCommandInfo) -> Self {
        if raw.__raw_names.is_empty() {
            Self::RootArgs(ArgsInfo::from_raw(&raw.__subcommands[0]))
        } else {
            Self::Subcommand(SubcommandInfo::from_raw(raw))
        }
    }
}

/// Description of a subcommand enum.
#[derive(Debug, Clone, Copy)]
pub struct SubcommandInfo(&'static RawCommandInfo);

impl SubcommandInfo {
    pub(crate) fn from_raw(raw: &'static RawCommandInfo) -> Self {
        Self(raw)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&'static str, ArgsInfo)> {
        std::iter::zip(
            self.0.__raw_names.split_terminator('\t'),
            self.0.__subcommands.iter().map(ArgsInfo::from_raw),
        )
    }

    pub fn get(&self, subcmd: &str) -> Option<ArgsInfo> {
        Some(self.iter().find(|(name, _)| *name == subcmd)?.1)
    }
}

/// Help and documentation of a command applet.
#[derive(Debug, Clone, Copy)]
pub struct CommandDoc {
    name: &'static str,
    version: Option<&'static str>,
    author: Option<&'static str>,
    about: Option<&'static str>,
    long_about: Option<&'static str>,
    after_help: Option<&'static str>,
    after_long_help: Option<&'static str>,
}

impl CommandDoc {
    pub fn name(&self) -> &str {
        self.name
    }

    pub fn version(&self) -> Option<&str> {
        self.version
    }

    pub fn author(&self) -> Option<&str> {
        self.author
    }

    pub fn about(&self) -> Option<&str> {
        self.about
    }

    pub fn long_about(&self) -> Option<&str> {
        self.long_about
    }

    pub fn after_help(&self) -> Option<&str> {
        self.after_help
    }

    pub fn after_long_help(&self) -> Option<&str> {
        self.after_long_help
    }
}
