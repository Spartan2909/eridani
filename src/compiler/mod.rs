use core::{fmt, result};

#[cfg(feature = "no_std")]
use core::error;

#[cfg(not(feature = "no_std"))]
use std::error;

use crate::prelude::*;

pub(crate) mod analyser;
pub(crate) mod parser;
pub(crate) mod scanner;

macro_rules! internal_error {
    ( $str:expr, $( $arg:expr )* ) => {
        panic!(concat!("internal compiler error: ", $str), $( $arg )*)
    };
}

pub(crate) use internal_error;

#[derive(Debug, Clone)]
pub enum Error {
    Collection(Vec<Error>),
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
            } => {
                if *line == usize::MAX {
                    write!(f, "[line <native>] {} Error{}: {}", kind, location, message)
                } else {
                    write!(f, "[line {}] {} Error{}: {}", line, kind, location, message)
                }
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
    pub(crate) fn new(line: usize, kind: &'static str, location: &str, message: &str) -> Self {
        Error::Single {
            line,
            kind,
            location: location.to_string(),
            message: message.to_string(),
        }
    }
}

type Result<T> = result::Result<T, Error>;

#[cfg(feature = "tree_walk")]
pub use analyser::Function;

#[cfg(feature = "tree_walk")]
pub fn parse(source: &str, entry_point: &str) -> Result<Vec<Rc<Function>>> {
    let tokens = scanner::scan(source)?;
    let parse_tree = parser::parse(tokens)?;
    analyser::analyse(parse_tree, entry_point)
}

pub fn compile(source: &str) -> Result<()> {
    let tokens = scanner::scan(source)?;

    let _ = parser::parse(tokens);

    Ok(())
}
