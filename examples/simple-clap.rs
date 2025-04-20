use clap::Parser;

#[allow(dead_code)]
#[path = "./util/simple.rs"]
mod cli;

fn main() {
    let cli = cli::Cli::parse();
    std::hint::black_box(&cli);
}
