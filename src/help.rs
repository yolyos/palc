use std::fmt::Write;

use crate::refl::{CommandInfo, RawCommandInfo};

/// List of (arg_info, subcommand) context, in reverse order.
/// The last element (outermost command) is the top-level command.
pub(crate) type SubcommandPath = Vec<(&'static RawCommandInfo, String)>;

pub(crate) fn generate(rev_path: &SubcommandPath, out: &mut String) -> std::fmt::Result {
    let path = rev_path
        .iter()
        .rev()
        .filter_map(|(raw, cmd)| match CommandInfo::from_raw(raw) {
            CommandInfo::RootArgs(args) => Some((cmd.as_str(), args)),
            CommandInfo::Subcommand(subcmds) => Some((cmd, subcmds.get(cmd)?)),
        })
        .collect::<Vec<_>>();

    // There must be at least a top-level `Parser` info, or we would fail fast by `MissingArg0`.
    assert!(!path.is_empty());
    let info = path.last().unwrap().1;

    // About this (sub)command.
    let cmd_doc = info.doc().expect("doc is enabled");
    if let Some(about) = cmd_doc.long_about() {
        writeln!(out, "{about}")?;
    }
    writeln!(out)?;

    // Usage of current subcommand path.

    write!(out, "Usage:")?;
    // Argv0 is included.
    for (cmd, _) in path.iter() {
        write!(out, " {cmd}")?;
    }

    let mut has_named @ mut has_unnamed = false;
    // TODO: Hide optional arguments?
    for arg in info.named_args() {
        has_named = true;
        if let Some(name) = arg.long_names().next() {
            write!(out, " --{name}")?;
        } else {
            write!(out, " -{}", arg.short_names().next().unwrap())?;
        }
        // TODO: Multi-value.
        if let Some(value_name) = arg.value_names().next() {
            let sep = if arg.requires_eq() { "=" } else { " " };
            write!(out, "{sep}<{value_name}>")?;
        }
    }
    for arg in info.unnamed_args() {
        has_unnamed = true;
        if arg.greedy() {
            write!(out, " [{}]...", arg.value_names().next().unwrap())?;
        } else {
            for value_name in arg.value_names() {
                write!(out, " <{value_name}>")?;
            }
        }
    }
    let subcmd = info.subcommand();
    if subcmd.is_some() {
        out.write_str(if info.is_subcommand_optional() { " [COMMAND]" } else { " <COMMAND>" })?;
    }
    writeln!(out)?;

    // List of commands.

    if let Some(subcmd) = &subcmd {
        writeln!(out, "\nCommands:")?;
        for (cmd, _) in subcmd.iter() {
            // TODO: Description.
            writeln!(out, "    {cmd}")?;
        }
    }

    // List of unnamed arguments.

    if has_unnamed {
        writeln!(out, "\nArguments:")?;
        for (i, arg) in info.unnamed_args().enumerate() {
            if i > 0 {
                writeln!(out)?;
            }
            for value_name in arg.value_names() {
                writeln!(out, "  <{value_name}>")?;
            }
            if let Some(help) = arg.long_help() {
                for (j, s) in help.split_terminator('\n').enumerate() {
                    if j > 0 {
                        writeln!(out)?;
                    }
                    writeln!(out, "          {s}")?;
                }
            }
        }
    }

    // List of named arguments.

    if has_named {
        writeln!(out, "\nOptions:")?;

        for arg in info.named_args() {
            match (arg.short_names().next(), arg.long_names().next()) {
                (None, None) => unreachable!(),
                (None, Some(long)) => write!(out, "      --{long}"),
                (Some(short), None) => write!(out, "  -{short}"),
                (Some(short), Some(long)) => write!(out, "  -{short}, --{long}"),
            }?;
            // TODO: Multi-value.
            if let Some(value_name) = arg.value_names().next() {
                let sep = if arg.requires_eq() { "=" } else { " " };
                write!(out, "{sep}<{value_name}>")?;
            }
            writeln!(out)?;
            if let Some(help) = arg.long_help() {
                for (j, s) in help.split_terminator('\n').enumerate() {
                    if j != 0 {
                        writeln!(out)?;
                    }
                    writeln!(out, "          {s}")?;
                }
            }
            writeln!(out)?;
        }
    }

    if let Some(after) = cmd_doc.after_long_help() {
        out.write_str(after)?;
    }

    Ok(())
}
