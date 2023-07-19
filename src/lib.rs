#![cfg_attr(not(feature = "std"), no_std, feature(error_in_core))]
#![warn(
    clippy::cast_lossless,
    clippy::semicolon_if_nothing_returned,
    clippy::semicolon_outside_block,
    clippy::significant_drop_tightening,
    clippy::std_instead_of_core,
    clippy::std_instead_of_core
)]
#![deny(clippy::dbg_macro)]
#![forbid(
    unsafe_op_in_unsafe_fn,
    clippy::multiple_unsafe_ops_per_block,
    clippy::todo,
    clippy::undocumented_unsafe_blocks
)]
#![cfg_attr(feature = "tree_walk", allow(dead_code))]

#[cfg(not(any(feature = "compiler", feature = "runtime")))]
compile_error!("Either feature 'compiler' or feature 'runtime' must be enabled");

#[cfg(all(feature = "tree_walk", any(feature = "web", feature = "target_web")))]
compile_error!("The treewalk interpreter cannot be used with web features");

#[cfg(all(feature = "runtime", feature = "tree_walk"))]
compile_error!("The treewalk interpreter cannot be used with the runtime");

extern crate alloc;

mod prelude {
    pub use alloc::{
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

#[cfg(all(feature = "compiler", not(feature = "tree_walk")))]
pub use compiler::compile;

#[cfg(feature = "runtime")]
mod runtime;

#[cfg(feature = "runtime")]
pub use runtime::run;
