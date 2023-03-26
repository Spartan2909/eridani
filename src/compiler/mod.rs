use core::{fmt, result};

#[cfg(feature = "no_std")]
use core::error;

#[cfg(not(feature = "no_std"))]
use std::error;

use crate::prelude::*;

mod scanner;
mod parser;

#[derive(Debug, Clone)]
pub enum Error {
    Collection(Vec<Error>),
    Single {
        line: usize,
        kind: &'static str,
        location: String,
        message: &'static str,
    },
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Collection(errors) => {
                write!(f, "{:#?}", errors)
            }
            Self::Single { line, kind, location, message } => {
                write!(
                    f,
                    "[line {}] {} Error{}: {}",
                    line, kind, location, message
                )
            }
        }
    }
}

impl From<Vec<Error>> for Error {
    fn from(value: Vec<Error>) -> Self {
        Self::Collection(value)
    }
}

impl error::Error for Error {}

impl Error {
    fn new(line: usize, kind: &'static str, location: &str, message: &'static str) -> Self {
        Error::Single { line, kind, location: location.to_string(), message }
    }
}

type Result<T> = result::Result<T, Error>;

#[cfg(feature = "tree_walk")]
pub use parser::Program;

#[cfg(feature = "tree_walk")]
pub fn parse(source: &str) -> Result<Program> {
    let tokens = dbg!(scanner::scan(source)?);
    parser::parse(tokens)
}

pub fn compile(source: &str) -> Result<()> {
    let tokens = scanner::scan(source)?;

    let _ = parser::parse(tokens);

    Ok(())
}
