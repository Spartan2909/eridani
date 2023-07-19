#[cfg(not(feature = "tree_walk"))]
pub(crate) mod bytecode;
#[cfg(not(feature = "tree_walk"))]
pub(crate) mod discriminant;
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

#[cfg(not(feature = "tree_walk"))]
pub type EridaniFunction = fn(&[Value]) -> Result<Value, ArgumentError>;

#[cfg(feature = "tree_walk")]
pub(crate) type EridaniFunction = fn(&[Value]) -> Result<Value, ArgumentError>;

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
    ( $( $tokens:expr ),* ) => {
        unsafe { ::core::hint::unreachable_unchecked() }
    };
}

macro_rules! expect_option {
    ( $value:expr ) => {
        $value.expect("internal compiler error")
    };

    ( $value:expr, $( $arg:tt ),+ ) => {
        $value.unwrap_or_else(|| $crate::common::internal_error!( $( $arg ),+ ))
    }
}

pub(crate) use internal_error;

pub(crate) use expect_option;
