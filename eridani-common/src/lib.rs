#![no_std]

pub mod bytecode;
pub mod discriminant;
pub mod natives;
pub mod value;

extern crate alloc;

use alloc::string::String;

#[cfg(feature = "std")]
extern crate std;

#[derive(Debug, Clone)]
pub struct ArgumentError {
    description: String,
}

impl ArgumentError {
    #[must_use]
    pub fn new(description: &str) -> ArgumentError {
        ArgumentError {
            description: description.into(),
        }
    }

    #[must_use]
    pub fn description(&self) -> &str {
        &self.description
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
pub enum RustChannel {
    Dev,
    Nightly,
    Beta,
    Stable,
}

#[derive(Debug, Clone, Copy, Eq)]
#[allow(dead_code)]
pub struct RustVersionData {
    pub major: u64,
    pub minor: u64,
    pub patch: u64,
    pub pre: &'static str,
    pub channel: RustChannel,
}

impl PartialEq for RustVersionData {
    fn eq(&self, other: &Self) -> bool {
        self.channel == other.channel && self.major == other.major && self.minor == other.minor
    }
}

#[derive(Debug, Clone, Copy, Eq)]
#[allow(dead_code)]
pub struct VersionData {
    pub rust_version: RustVersionData,
    pub major: u64,
    pub minor: u64,
    pub patch: u64,
}

impl PartialEq for VersionData {
    fn eq(&self, other: &Self) -> bool {
        self.rust_version == other.rust_version
            && self.major == other.major
            && self.minor == other.minor
    }
}

#[macro_export]
macro_rules! internal_error {
    () => {
        panic!("internal compiler error")
    };

    ( $str:expr_2021 ) => {
        panic!(concat!("internal compiler error: ", $str))
    };

    ( $str:expr_2021, $( $arg:expr_2021 ),+ ) => {
        panic!(concat!("internal compiler error: ", $str), $( $arg ),+)
    };
}

#[macro_export]
macro_rules! expect_option {
    ( $value:expr_2021 ) => {
        $value.expect("internal compiler error")
    };

    ( $value:expr_2021, $( $arg:tt ),+ ) => {
        $value.unwrap_or_else(|| $crate::internal_error!( $( $arg ),+ ))
    }
}
