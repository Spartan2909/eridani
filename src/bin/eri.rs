use std::{fs, process::ExitCode};

#[cfg(debug_assertions)]
use std::time::Instant;

use clap::Parser;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// The file to compile and run
    file_path: String,

    /// Compile the given file without running it
    #[arg(short, long = "no-run")]
    no_run: bool,

    /// Run a previously compiled file
    #[arg(short, long)]
    compiled: bool,

    /// The function to start from (defaults to 'main')
    #[arg(long = "entry-point")]
    entry_point: Option<String>,
}

fn main() -> ExitCode {
    let args = Args::parse();
    let entry_point = if let Some(string) = args.entry_point {
        string
    } else {
        "main".to_string()
    };

    let file_path =
        fs::canonicalize(&args.file_path).expect("Should have been able to read the file");
    let contents = fs::read_to_string(&file_path).expect("Should have been able to read the file");

    #[cfg(debug_assertions)]
    let start = Instant::now();

    let program = match eridani::parse(
        &contents,
        Some(file_path.to_string_lossy().into()),
        &entry_point,
    ) {
        Ok(tree) => tree,
        Err(e) => {
            eprintln!("{e}");
            return ExitCode::FAILURE;
        }
    };

    match eridani::walk_tree(program, &[]) {
        Ok(value) => {
            if value.is_something() {
                println!("{value}")
            }
        }
        Err(e) => {
            eprintln!("{e}");
            return ExitCode::FAILURE;
        }
    }

    #[cfg(debug_assertions)]
    println!("time taken: {}ms", start.elapsed().as_millis());

    ExitCode::SUCCESS
}
