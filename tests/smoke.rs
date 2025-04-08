use std::path::PathBuf;

use clap_static::{Parser, Subcommand, ValueEnum};

#[derive(Debug, PartialEq, Parser)]
struct MyCli {
    name: Option<String>,

    #[arg(short, long)]
    config: PathBuf,

    #[arg(short = 'v')]
    debug: bool,

    #[arg(long)]
    color: Color,

    #[command(subcommand)]
    command: Option<Commands>,
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
            config: "foo".into(),
            debug: false,
            command: Some(Commands::Test {
                list: true,
                files: vec![PathBuf::from("hello"), PathBuf::from("world")],
            }),
        }
    );
}
