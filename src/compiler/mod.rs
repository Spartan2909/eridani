use core::{fmt, result};

mod arena;
#[cfg(not(feature = "tree_walk"))]
mod bytecode;
mod eridani_std;
pub(crate) mod ir;
pub(crate) mod parser;
pub(crate) mod scanner;

use crate::prelude::*;

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
                write!(f, "{:#?}", errors)
            }
            Self::Single {
                line,
                kind,
                location,
                message,
            } => write!(f, "[line {}] {} Error{}: {}", line, kind, location, message),
        }
    }
}

impl From<Vec<Error>> for Error {
    fn from(value: Vec<Error>) -> Self {
        Self::Collection(value)
    }
}

#[cfg(all(not(feature = "std"), feature = "error_trait"))]
impl core::error::Error for Error {}

#[cfg(feature = "std")]
impl std::error::Error for Error {}

impl Error {
    pub(crate) fn new(line: usize, kind: &'static str, location: &str, message: &str) -> Self {
        Error::Single {
            line,
            kind,
            location: location.to_string(),
            message: message.to_string(),
        }
    }
}

pub type Result<T> = result::Result<T, Error>;

#[cfg(not(feature = "tree_walk"))]
use crate::common::bytecode::Program;

#[cfg(not(feature = "tree_walk"))]
pub fn compile(
    source: String,
    source_origin: Option<String>,
    entry_point: &str,
) -> Result<Program> {
    let tokens = scanner::scan(source)?;
    let parse_tree = parser::parse(tokens)?;
    let arena = arena::Arena::new();
    let analysed = ir::analyse(&arena, parse_tree, source_origin, entry_point)?;
    Ok(bytecode::compile(analysed))
}
