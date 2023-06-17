use core::{fmt, result};

#[cfg(not(feature = "std"))]
use core::error;

#[cfg(feature = "std")]
use std::error;

use crate::common::ArgumentError;

#[cfg(feature = "tree_walk")]
pub(crate) mod treewalk;

#[derive(Debug, Clone)]
pub enum ErrorInner {
    Argument(ArgumentError),
    Generic {
        kind: String,
        message: String,
        line: Option<usize>,
    },
}

#[derive(Debug, Clone)]
pub struct Error {
    inner: ErrorInner,
    trace: Vec<(String, usize, usize)>,
}

impl Error {
    pub fn from_argument_error(error: ArgumentError, function_name: &str) -> Self {
        Error {
            inner: ErrorInner::Argument(error),
            trace: vec![(function_name.to_string(), usize::MAX, 1)],
        }
    }

    pub fn new(kind: &str, message: &str, line: usize) -> Self {
        Error {
            inner: ErrorInner::Generic {
                kind: kind.to_owned(),
                message: message.to_owned(),
                line: Some(line),
            },
            trace: vec![],
        }
    }

    fn extend_trace(&mut self, function_name: &str, line: usize) {
        if let Some((name, trace_line, count)) = self.trace.last_mut() {
            if function_name == name && *trace_line == line {
                *count += 1;
                return;
            }
        }

        self.trace.push((function_name.to_owned(), line, 1));
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (name, line, count) in self.trace.iter().rev() {
            if *line == usize::MAX {
                writeln!(f, "In {name}:")?;
            } else if *count > 1 {
                writeln!(f, "Line {line}, in {name}:    ({count})")?;
            } else {
                writeln!(f, "Line {line}, in {name}:")?;
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
    fn init_or_add_context(self, function_name: &str, line: usize) -> Self;
    fn pop_trace(self) -> Self;
}

impl<T> EridaniResult for Result<T> {
    fn extend_trace(self, function_name: &str, line: usize) -> Self {
        if let Err(mut e) = self {
            e.extend_trace(function_name, line);
            Err(e)
        } else {
            self
        }
    }

    fn init_or_add_context(mut self, function_name: &str, line: usize) -> Self {
        if let Err(e) = &mut self {
            if let ErrorInner::Generic { line, .. } = &mut e.inner {
                if let Some(actual_line) = *line {
                    *line = None;
                    e.extend_trace(function_name, actual_line);
                    return self;
                }
            }

            e.extend_trace(function_name, line);
        }

        self
    }

    fn pop_trace(mut self) -> Self {
        if let Err(e) = &mut self {
            e.trace.pop();
        }

        self
    }
}
