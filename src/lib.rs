#![feature(fs_try_exists, let_chains, test, try_blocks)]
#![cfg_attr(not(feature = "std"), no_std, feature(error_in_core))]
#![warn(
    clippy::cast_lossless,
    clippy::dbg_macro,
    clippy::std_instead_of_core,
    clippy::std_instead_of_core
)]

#[cfg(not(any(feature = "compiler", feature = "runtime")))]
compile_error!("Either feature 'compiler' or feature 'runtime' must be enabled.");

#[cfg(all(
    not(feature = "compiler"),
    any(feature = "target_std", feature = "target_web"),
))]
compile_error!("The 'target_*' features only affect the compiler.");

#[cfg(all(feature = "tree_walk", any(feature = "web", feature = "target_web")))]
compile_error!("The treewalk interpreter cannot be used with web features");

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

#[cfg(feature = "tree_walk")]
pub use treewalk::walk_tree;
