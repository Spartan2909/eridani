#![cfg_attr(not(feature = "std"), no_std, feature(error_in_core))]
#![warn(
    clippy::cast_lossless,
    clippy::dbg_macro,
    clippy::semicolon_if_nothing_returned,
    clippy::semicolon_outside_block,
    clippy::significant_drop_tightening,
    clippy::std_instead_of_core,
    clippy::std_instead_of_core
)]
#![forbid(
    unsafe_op_in_unsafe_fn,
    clippy::multiple_unsafe_ops_per_block,
    clippy::undocumented_unsafe_blocks
)]

#[cfg(not(any(feature = "compiler", feature = "runtime")))]
compile_error!("Either feature 'compiler' or feature 'runtime' must be enabled");

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
}

mod common;

#[cfg(feature = "compiler")]
mod compiler;

#[cfg(all(feature = "compiler", not(feature = "tree_walk")))]
pub use compiler::compile;

#[cfg(feature = "tree_walk")]
pub use compiler::parse;

#[cfg(feature = "runtime")]
mod runtime;


#[cfg(feature = "tree_walk")]
pub use runtime::treewalk::walk_tree;
