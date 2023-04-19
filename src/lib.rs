#![feature(let_chains, try_blocks)]
#![cfg_attr(feature = "no_std", no_std, feature(core_intrinsics, error_in_core))]

#[cfg(not(any(feature = "compiler", feature = "runtime")))]
compile_error!("Either feature 'compiler' or feature 'runtime' must be enabled.");

extern crate alloc;

mod prelude {
    pub use alloc::{
        boxed::Box,
        format,
        string::{String, ToString},
        vec,
        vec::Vec,
    };

    pub use core::prelude::rust_2021::*;
}

mod common;

#[cfg(feature = "compiler")]
pub mod compiler;

#[cfg(feature = "runtime")]
mod runtime;

#[cfg(feature = "tree_walk")]
mod treewalk;
