use std::fmt::Write;

use crate::refl::{ArgsInfo, CommandInfo};

pub(crate) type SubcommandPath = Vec<(String, CommandInfo)>;

pub(crate) fn generate(path: &SubcommandPath, out: &mut String) -> std::fmt::Result {
    write!(out, "Usage:")?;
    let ((last_cmd, cmd_info), ancestors) = path.split_first().unwrap();
    for (subcmd, _) in ancestors.iter().rev() {
        write!(out, " {subcmd}")?;
    }
    if let Some(args) = cmd_info.catchall().or_else(|| {
        cmd_info
            .commands()
            .find_map(|(c, args)| (c == last_cmd).then_some(args))
    }) {
        // Inside a valid subcommand.
        write!(out, " {last_cmd}")?;
        generate_for_args(args, out)?;
    } else {
        // Invalid last subcommand, help for all subcommands?
        todo!();
    }
    Ok(())
}

fn generate_for_args(info: &ArgsInfo, out: &mut String) -> std::fmt::Result {
    let mut has_named @ mut has_unnamed = false;

    // TODO: Hide optional arguments?
    for arg in info.named_args() {
        has_named = true;
        let name = arg
            .long_names()
            .first()
            .or(arg.short_names().first())
            .unwrap();
        write!(out, " {name}")?;
        match arg.value_display() {
            [] => {}
            [v] => {
                let sep = if arg.requires_eq() { "=" } else { " " };
                write!(out, "{sep}<{v}>")?;
            }
            _ => todo!(),
        }
    }
    for arg in info.unnamed_args() {
        has_unnamed = true;
        write!(out, " <{}>", arg.value_display())?;
    }
    if let Some((_, vaarg)) = info.trailing_var_arg() {
        write!(out, " [{}]...", vaarg.value_display())?;
    }
    if let Some((optional, _)) = info.subcommand() {
        out.write_str(if optional { " [COMMAND]" } else { " <COMMAND>" })?;
    }
    writeln!(out)?;

    if has_unnamed {
        writeln!(out, "\nArguments:")?;
        for arg in info.unnamed_args() {
            writeln!(out, "  <{}>", arg.value_display())?;
        }
    }

    if has_named {
        writeln!(out, "\nOptions:")?;

        for arg in info.named_args() {
            match (arg.short_names().first(), arg.long_names().first()) {
                (None, None) => unreachable!(),
                (None, Some(long)) => write!(out, "      {long}"),
                (Some(short), None) => write!(out, "  {short}"),
                (Some(short), Some(long)) => write!(out, "  {short}, {long}"),
            }?;
            writeln!(out)?;
        }
    }

    if let Some((_, cmd_info)) = info.subcommand() {
        writeln!(out, "\nCommands:")?;
        for (cmd, _) in cmd_info.commands() {
            writeln!(out, "    {cmd}")?;
        }
    }

    Ok(())
}
