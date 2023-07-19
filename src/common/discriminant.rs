#[cfg(feature = "serialise")]
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "serialise", derive(Serialize, Deserialize))]
pub struct TargetFeatures(u8);

#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "serialise", derive(Serialize, Deserialize))]
pub struct RuntimeFeatures(u8);

#[cfg(feature = "target_std")]
const TARGET_STD: u8 = 0b00000001;

#[cfg(not(feature = "target_std"))]
const TARGET_STD: u8 = 0;

#[cfg(feature = "target_web")]
const TARGET_WEB: u8 = 0b00000010;

#[cfg(not(feature = "target_web"))]
const TARGET_WEB: u8 = 0;

pub const TARGET_FEATURES: TargetFeatures = TargetFeatures(TARGET_STD | TARGET_WEB);

#[cfg(feature = "std")]
const RUNTIME_STD: u8 = 0b00000001;

#[cfg(not(feature = "std"))]
const RUNTIME_STD: u8 = 0;

#[cfg(feature = "web")]
const RUNTIME_WEB: u8 = 0b00000010;

#[cfg(not(feature = "web"))]
const RUNTIME_WEB: u8 = 0;

pub const RUNTIME_FEATURES: RuntimeFeatures = RuntimeFeatures(RUNTIME_STD | RUNTIME_WEB);

#[inline]
pub const fn features_compatible(
    target_features: TargetFeatures,
    runtime_features: RuntimeFeatures,
) -> bool {
    target_features.0 & runtime_features.0 == target_features.0
}
