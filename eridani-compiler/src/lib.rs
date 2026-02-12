#![no_std]

mod bytecode;
mod eridani_std;
pub(crate) mod ir;
pub(crate) mod parser;
pub(crate) mod scanner;
mod value;

use core::{fmt, result};

extern crate alloc;

use alloc::{borrow::ToOwned, string::String, vec::Vec};

#[cfg(feature = "std")]
extern crate std;

#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum Error {
    #[doc(hidden)]
    Collection(Vec<Error>),
    #[doc(hidden)]
    Single {
        line: usize,
        kind: &'static str,
        location: String,
        message: String,
    },
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Collection(errors) => {
                write!(f, "{errors:#?}")
            }
            Self::Single {
                line,
                kind,
                location,
                message,
            } => write!(f, "[line {line}] {kind} Error{location}: {message}"),
        }
    }
}

impl From<Vec<Error>> for Error {
    fn from(value: Vec<Error>) -> Self {
        Self::Collection(value)
    }
}

impl core::error::Error for Error {}

impl Error {
    pub(crate) fn new(line: usize, kind: &'static str, location: &str, message: &str) -> Self {
        Error::Single {
            line,
            kind,
            location: location.to_owned(),
            message: message.to_owned(),
        }
    }
}

pub type Result<T> = result::Result<T, Error>;

use eridani_common::bytecode::Program;

/// ## Errors
/// Returns an error if scanning, parsing, or name resolution fail, or if a runtime error is
/// detected ahead of time.
pub fn compile(
    source: String,
    source_origin: Option<String>,
    entry_point: &str,
) -> Result<Program> {
    let tokens = scanner::scan(&source)?;
    let parse_tree = parser::parse(tokens, source)?;
    let analysed = ir::analyse(&parse_tree, source_origin, entry_point)?;
    let code = bytecode::compile(&analysed);
    Ok(code)
}
