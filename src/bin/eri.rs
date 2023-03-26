use std::fs;

use eridani;

use clap::Parser;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// The file to run
    file_path: String,

    /// Compile the given file without running it
    #[arg(short, long = "no-run")]
    no_run: bool,

    /// Run a previously compiled file
    #[arg(short, long)]
    compiled: bool,
}

fn main() {
    let args = Args::parse();

    let contents = fs::read_to_string(args.file_path)
        .expect("Should have been able to read the file");

    dbg!(eridani::compiler::parse(&contents));
}
