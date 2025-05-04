//! This is an example for a dead simple program.
use std::path::PathBuf;

use clap_static::Parser;

#[expect(dead_code, reason = "for bench")]
#[derive(Parser)]
pub struct Cli {
    #[arg(long, short)]
    verbose: bool,
    file: PathBuf,
}

fn main() {
    let cli = Cli::parse();
    std::hint::black_box(cli);
}
