#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(feature = "error_trait", feature(error_in_core))]

#[cfg(not(any(feature = "compiler", feature = "runtime")))]
compile_error!("Either feature 'compiler' or feature 'runtime' must be enabled");

extern crate alloc;

mod prelude {
    pub use alloc::{
        borrow::ToOwned,
        boxed::Box,
        format,
        string::{String, ToString},
        vec,
        vec::Vec,
    };
}

#[cfg(feature = "ffi")]
pub mod ffi {
    pub use crate::common::{value::Value, ArgumentError, EridaniFunction, VersionData};

    use crate::common::{RustChannel, RustVersionData};

    include!(concat!(env!("OUT_DIR"), "/eridani_version_data.rs"));
}

mod common;

#[cfg(feature = "compiler")]
mod compiler;

#[cfg(feature = "compiler")]
pub use compiler::compile;

#[cfg(feature = "runtime")]
mod runtime;

#[cfg(feature = "runtime")]
pub use runtime::run;
