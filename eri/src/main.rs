use std::path::PathBuf;
use std::{fs, process::ExitCode};

//#[cfg(debug_assertions)]
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

    /// The function to start from (defaults to 'main')
    #[arg(long = "entry-point")]
    entry_point: Option<String>,

    /// An argument to be passed to the `main` function.
    /// Multiple values can specified with this option, and
    /// they will be passed in the given order
    #[arg(long = "arg")]
    #[allow(clippy::struct_field_names)]
    program_args: Vec<String>,
}

fn main() -> ExitCode {
    let args = Args::parse();

    let entry_point = args.entry_point.as_deref().unwrap_or("main");

    let Ok(program_args) = args.program_args.iter().map(|s| s.parse()).collect() else {
        eprintln!("Invalid program arguments '{:?}'", args.program_args);
        return ExitCode::FAILURE;
    };

    let file_path = PathBuf::from(args.file_path.clone());
    /*let mut bytecode_path = file_path.clone();
    bytecode_path.set_extension("eric");

    let (start, program, cached) = if let Ok(contents) = fs::read(&bytecode_path) {
        (Instant::now(), from_bytes(&contents).unwrap(), None)
    } else {
        let contents = fs::read_to_string(&file_path).expect("Should have been able to read the file");
        let start = Instant::now();
        let program = match eridani::compile(
            contents,
            Some(args.file_path),
            entry_point,
        ) {
            Ok(tree) => tree,
            Err(e) => {
                eprintln!("{e}");
                return ExitCode::FAILURE;
            }
        };
        let cached = to_allocvec(&program).unwrap();
        (start, program, Some(cached))
    };*/

    let contents = fs::read_to_string(file_path).expect("Should have been able to read the file");
    let start = Instant::now();
    let program = match eridani::compile(contents, Some(args.file_path), entry_point) {
        Ok(tree) => tree,
        Err(e) => {
            eprintln!("{e}");
            return ExitCode::FAILURE;
        }
    };

    match eridani::run(program, program_args) {
        Ok(value) => {
            if value.is_something() {
                println!("{value}");
            }
        }
        Err(e) => {
            eprintln!("{e}");
            return ExitCode::FAILURE;
        }
    }

    //#[cfg(debug_assertions)]
    println!("time taken: {}ms", start.elapsed().as_millis());

    /*if let Some(bytecode) = cached {
        let mut file = File::create(bytecode_path).unwrap();
        file.write_all(&bytecode).unwrap();
    }*/

    ExitCode::SUCCESS
}
