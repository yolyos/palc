use clap_static::{Args, Parser, Subcommand, ValueEnum};

#[path = "./util/deno.rs"]
mod cli;

fn main() {
    let cli = cli::Opt::parse();
    std::hint::black_box(&cli);
}
