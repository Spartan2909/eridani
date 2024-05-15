use core::{fmt, result};

#[cfg(all(not(feature = "std"), feature = "error_trait"))]
use core::error;

#[cfg(all(feature = "std", feature = "error_trait"))]
use std::error;

use crate::{
    common::{bytecode::Program, discriminant, value::Value, ArgumentError},
    prelude::*,
};

mod vm;

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
    pub fn from_argument_error(error: ArgumentError, function_name: String) -> Self {
        Error {
            inner: ErrorInner::Argument(error),
            trace: vec![(function_name, usize::MAX, 1)],
        }
    }

    pub fn new(kind: String, message: String, line: usize) -> Self {
        Error {
            inner: ErrorInner::Generic {
                kind,
                message,
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

    fn init_or_add_context(mut self, function_name: &str, line: usize) -> Self {
        if let ErrorInner::Generic { line, .. } = &mut self.inner {
            if let Some(actual_line) = *line {
                *line = None;
                self.extend_trace(function_name, actual_line);
                return self;
            }
        }

        self.extend_trace(function_name, line);

        self
    }

    fn pop_trace(mut self) -> Self {
        self.trace.pop();

        self
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

#[cfg(feature = "error_trait")]
impl error::Error for Error {}

pub type Result<T> = result::Result<T, Error>;

struct ArgsFormatter<'a>(&'a [Value]);

impl fmt::Display for ArgsFormatter<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        for (i, arg) in self.0.iter().enumerate() {
            if arg.is_string() {
                write!(f, "'{arg}'")?;
            } else {
                write!(f, "{arg}")?;
            }
            if i < self.0.len() - 1 {
                write!(f, ",")?;
            }
        }
        write!(f, ")")?;
        Ok(())
    }
}

/// ## Errors
/// Returns any errors encountered by the program.
pub fn run(program: Program, args: Vec<Value>) -> Result<Value> {
    if !discriminant::compatible_with_runtime(&program) {
        return Err(Error::new(
            "Compatability".to_string(),
            "Not all expected features are enabled in the runtime".to_string(),
            0,
        ));
    }
    let mut vm = vm::Vm::new(program);
    vm.run(args)
}
