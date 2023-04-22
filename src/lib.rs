#![feature(let_chains, try_blocks)]
#![cfg_attr(feature = "no_std", no_std, feature(error_in_core))]
#![warn(
    clippy::cast_lossless,
    clippy::dbg_macro,
    clippy::std_instead_of_core,
    clippy::std_instead_of_core
)]

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
mod compiler;

#[cfg(feature = "compiler")]
pub use compiler::compile;

#[cfg(feature = "tree_walk")]
pub use compiler::{parse, Function};

#[cfg(feature = "runtime")]
mod runtime;

#[cfg(feature = "tree_walk")]
mod treewalk;
