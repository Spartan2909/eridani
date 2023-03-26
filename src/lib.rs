#![cfg_attr(feature = "no_std", no_std, feature(error_in_core))]

#[cfg(not(any(feature = "compiler", feature = "runtime")))]
compile_error!("Either feature 'compiler' or feature 'runtime' must be enabled.");

mod prelude {
    extern crate alloc;
    pub use alloc::{string::{String, ToString}, vec, vec::Vec};

    pub use core::prelude::rust_2021::*;
}

mod common;

#[cfg(feature = "compiler")]
pub mod compiler;

#[cfg(feature = "runtime")]
mod runtime;

#[cfg(feature = "tree_walk")]
mod treewalk;
