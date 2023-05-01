use core::{fmt, result};

#[cfg(not(feature = "std"))]
use core::error;

#[cfg(feature = "std")]
use std::error;

use crate::{common::ArgumentError, prelude::*};

#[derive(Debug, Clone)]
pub enum ErrorInner {
    Argument(ArgumentError),
    Generic {
        kind: String,
        message: String,
        line: usize,
    },
}

#[derive(Debug, Clone)]
pub struct Error {
    inner: ErrorInner,
    trace: Vec<(String, usize)>,
}

impl Error {
    pub fn from_argument_error(error: ArgumentError, function_name: &str) -> Self {
        Error {
            inner: ErrorInner::Argument(error),
            trace: vec![(function_name.to_string(), usize::MAX)],
        }
    }

    pub fn new(kind: &str, message: &str, line: usize) -> Self {
        Error {
            inner: ErrorInner::Generic {
                kind: kind.to_owned(),
                message: message.to_owned(),
                line,
            },
            trace: vec![],
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (name, line) in self.trace.iter().rev() {
            if *line == usize::MAX {
                writeln!(f, "In {}:", name)?;
            } else {
                writeln!(f, "Line {}, in {}:", line, name)?;
            }
        }

        match &self.inner {
            ErrorInner::Argument(e) => write!(f, "Argument Error: {}", e.description()),
            ErrorInner::Generic { kind, message, .. } => write!(f, "{kind} Error: {message}"),
        }
    }
}

impl error::Error for Error {}

pub type Result<T> = result::Result<T, Error>;

pub trait EridaniResult {
    fn extend_trace(self, function_name: &str, line: usize) -> Self;
    fn add_function_name(self, function_name: &str) -> Self;
}

impl<T> EridaniResult for Result<T> {
    fn extend_trace(self, function_name: &str, line: usize) -> Self {
        if let Err(mut e) = self {
            e.trace.push((function_name.to_owned(), line));
            Err(e)
        } else {
            self
        }
    }

    fn add_function_name(mut self, function_name: &str) -> Self {
        if let Err(e) = &mut self {
            if let ErrorInner::Generic { line, .. } = &e.inner {
                let line = *line;
                e.trace.push((function_name.to_owned(), line));
            }
        }

        self
    }
}
