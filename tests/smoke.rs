use std::path::PathBuf;

use clap_static::{Args, Parser, Subcommand, ValueEnum};

#[derive(Debug, PartialEq, Parser)]
struct MyCli {
    name: Option<String>,

    #[command(flatten)]
    config: Config,

    #[arg(short = 'v')]
    debug: bool,

    #[arg(long)]
    color: Color,

    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Debug, PartialEq, Args)]
struct Config {
    #[arg(long)]
    config_file: Option<PathBuf>,
    #[arg(long)]
    config: Option<String>,
}

#[derive(Debug, PartialEq, Subcommand)]
enum Commands {
    Test {
        #[arg(short, long)]
        list: bool,

        files: Vec<PathBuf>,
    },
}

#[derive(Debug, PartialEq, ValueEnum)]
enum Color {
    Auto,
    Never,
    Always,
}

#[test]
fn smoke() {
    let args = MyCli::try_parse_from([
        "foo",
        "--color=always",
        "--config",
        "foo",
        "bar",
        "test",
        "-l",
        "hello",
        "world",
    ])
    .unwrap();
    assert_eq!(
        args,
        MyCli {
            color: Color::Always,
            name: Some("bar".into()),
            config: Config {
                config_file: None,
                config: Some("foo".into())
            },
            debug: false,
            command: Some(Commands::Test {
                list: true,
                files: vec![PathBuf::from("hello"), PathBuf::from("world")],
            }),
        }
    );
}
