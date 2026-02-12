#![no_std]

extern crate alloc;

#[cfg(feature = "ffi")]
pub mod ffi {
    pub use eridani_common::{natives::NativeFunction, value::Value, ArgumentError, VersionData};

    use eridani_common::{RustChannel, RustVersionData};

    include!(concat!(env!("OUT_DIR"), "/eridani_version_data.rs"));
}

#[cfg(feature = "compiler")]
pub use eridani_compiler as compiler;

#[cfg(feature = "runtime")]
pub use eridani_runtime as runtime;
