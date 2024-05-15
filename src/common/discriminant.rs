use super::bytecode::Program;

#[cfg(feature = "serialise")]
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "serialise", derive(Serialize, Deserialize))]
pub struct TargetFeatures(u8);

#[cfg(feature = "target_std")]
const TARGET_STD: u8 = 0b0000_0001;

#[cfg(not(feature = "target_std"))]
const TARGET_STD: u8 = 0;

#[cfg(feature = "target_web")]
const TARGET_WEB: u8 = 0b0000_0010;

#[cfg(not(feature = "target_web"))]
const TARGET_WEB: u8 = 0;

pub const TARGET_FEATURES: TargetFeatures = TargetFeatures(TARGET_STD | TARGET_WEB);

#[cfg(feature = "std")]
const RUNTIME_STD: u8 = 0b0000_0001;

#[cfg(not(feature = "std"))]
const RUNTIME_STD: u8 = 0;

#[cfg(feature = "web")]
const RUNTIME_WEB: u8 = 0b0000_0010;

#[cfg(not(feature = "web"))]
const RUNTIME_WEB: u8 = 0;

const RUNTIME_FEATURES: u8 = RUNTIME_STD | RUNTIME_WEB;

pub const fn compatible_with_runtime(program: &Program) -> bool {
    program.features.0 & RUNTIME_FEATURES == program.features.0
}
