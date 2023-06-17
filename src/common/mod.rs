#[cfg(not(feature = "tree_walk"))]
mod discriminant;
pub(crate) mod natives;
pub(crate) mod value;
use value::Value;

#[derive(Debug, Clone)]
pub struct ArgumentError {
    description: String,
}

impl ArgumentError {
    pub fn new(description: &str) -> ArgumentError {
        ArgumentError {
            description: description.to_string(),
        }
    }

    pub fn description(&self) -> &String {
        &self.description
    }
}

pub fn get(args: &[Value], index: usize) -> Result<Value, ArgumentError> {
    if let Some(value) = args.get(index) {
        Ok(value.clone())
    } else {
        let description = format!("No item at index '{index}'");
        Err(ArgumentError::new(&description))
    }
}

pub fn get_string(args: &[Value], index: usize) -> Result<String, ArgumentError> {
    let value = get(args, index)?;
    match value {
        Value::String(s) => Ok(s),
        _ => {
            let description = format!("Expected a string, found '{value}'");
            Err(ArgumentError::new(&description))
        }
    }
}

/// A function that can be called from eridani code.
///
/// Automatically implemented for Rust functions and `FnMut` closures.
pub trait EridaniFunction: FnMut(&[Value]) -> Result<Value, ArgumentError> {
    /// Returns a `Box` containing a clone of this value.
    fn clone_box<'a>(&self) -> Box<dyn 'a + EridaniFunction>
    where
        Self: 'a;
}

impl<F> EridaniFunction for F
where
    F: FnMut(&[Value]) -> Result<Value, ArgumentError> + Clone,
{
    fn clone_box<'a>(&self) -> Box<dyn 'a + EridaniFunction>
    where
        Self: 'a,
    {
        Box::new(self.clone())
    }
}

impl<'a> Clone for Box<dyn 'a + EridaniFunction> {
    fn clone(&self) -> Self {
        (**self).clone_box()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
pub(crate) enum RustChannel {
    Dev,
    Nightly,
    Beta,
    Stable,
}

#[derive(Debug, Clone, Copy, Eq)]
#[allow(dead_code)]
pub(crate) struct RustVersionData {
    pub(crate) major: u64,
    pub(crate) minor: u64,
    pub(crate) patch: u64,
    pub(crate) pre: &'static str,
    pub(crate) channel: RustChannel,
}

impl PartialEq for RustVersionData {
    fn eq(&self, other: &Self) -> bool {
        self.channel == other.channel && self.major == other.major && self.minor == other.minor
    }
}

#[derive(Debug, Clone, Copy, Eq)]
#[allow(dead_code)]
pub struct VersionData {
    pub(crate) rust_version: RustVersionData,
    pub(crate) major: u64,
    pub(crate) minor: u64,
    pub(crate) patch: u64,
}

impl PartialEq for VersionData {
    fn eq(&self, other: &Self) -> bool {
        self.rust_version == other.rust_version
            && self.major == other.major
            && self.minor == other.minor
    }
}

#[cfg(debug_assertions)]
macro_rules! internal_error {
    () => {
        panic!("internal compiler error")
    };

    ( $str:expr ) => {
        panic!(concat!("internal compiler error: ", $str))
    };

    ( $str:expr, $( $arg:expr ),+ ) => {
        panic!(concat!("internal compiler error: ", $str), $( $arg ),+)
    };
}

#[cfg(not(debug_assertions))]
macro_rules! internal_error {
    ( $( $tokens:expr )* ) => {
        panic!("internal compiler error")
    };
}

pub(crate) use internal_error;
