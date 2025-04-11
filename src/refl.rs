#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ArgsInfo {
    direct_named_args: &'static [NamedArgInfo],
    direct_unnamed_args: &'static [UnnamedArgInfo],
    last_arg: Option<UnnamedArgInfo>,
    flatten_args: &'static [&'static ArgsInfo],
    total_named_arg_cnt: usize,
    total_unnamed_arg_cnt: usize,
}

impl ArgsInfo {
    #[doc(hidden)]
    pub const fn new(
        direct_named_args: &'static [NamedArgInfo],
        direct_unnamed_args: &'static [UnnamedArgInfo],
        mut last_arg: Option<UnnamedArgInfo>,
        flatten_args: &'static [&'static ArgsInfo],
    ) -> Self {
        let mut total_named_arg_cnt = direct_named_args.len();
        let mut total_unnamed_arg_cnt = direct_unnamed_args.len();
        let mut i = 0;
        while i < flatten_args.len() {
            let flatten = flatten_args[i];
            total_named_arg_cnt += flatten.total_named_arg_cnt;
            total_unnamed_arg_cnt += flatten.total_unnamed_arg_cnt;
            if let Some(last) = flatten.last_arg {
                if last_arg.replace(last).is_some() {
                    panic!("Args and command(flatten) Args cannot both have arg(last)");
                }
            }
            i += 1;
        }

        Self {
            direct_named_args,
            direct_unnamed_args,
            last_arg,
            flatten_args,
            total_named_arg_cnt,
            total_unnamed_arg_cnt,
        }
    }
}

impl ArgsInfo {
    pub(crate) const fn empty() -> Self {
        Self {
            direct_named_args: &[],
            direct_unnamed_args: &[],
            last_arg: None,
            flatten_args: &[],
            total_named_arg_cnt: 0,
            total_unnamed_arg_cnt: 0,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct NamedArgInfo {
    long_names: &'static [&'static str],
    short_names: &'static [&'static str],
    value_display: &'static str,
    require_eq: bool,
}

impl NamedArgInfo {
    #[doc(hidden)]
    pub const fn new(
        long_names: &'static [&'static str],
        short_names: &'static [&'static str],
        value_display: &'static str,
        require_eq: bool,
    ) -> Self {
        Self {
            long_names,
            short_names,
            value_display,
            require_eq,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct UnnamedArgInfo {
    value_display: &'static str,
}

impl UnnamedArgInfo {
    #[doc(hidden)]
    pub const fn new(value_display: &'static str) -> Self {
        Self { value_display }
    }
}
