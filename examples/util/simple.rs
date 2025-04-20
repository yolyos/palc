//! This is an example for a dead simple program.
use std::path::PathBuf;

use super::Parser;

#[derive(Parser)]
pub struct Cli {
    #[arg(long, short)]
    verbose: bool,
    file: PathBuf,
}
