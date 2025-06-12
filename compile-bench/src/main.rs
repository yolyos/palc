//! This is an example for a dead simple program.
use std::path::PathBuf;

use palc::Parser;

/// My simple CLI.
#[expect(dead_code, reason = "for bench")]
#[derive(Parser)]
pub struct Cli {
    /// Print more.
    #[arg(long, short)]
    verbose: bool,
    /// The input file path.
    file: PathBuf,
}

fn main() {
    let cli = Cli::parse();
    std::hint::black_box(cli);
}
