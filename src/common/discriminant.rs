#![allow(dead_code)] // remove this when bytecode interpreter is finished

#[cfg(feature = "target_std")]
const TARGET_STD: u8 = 0x00000001;

#[cfg(not(feature = "target_std"))]
const TARGET_STD: u8 = 0;

#[cfg(feature = "target_web")]
const TARGET_WEB: u8 = 0b00000010;

#[cfg(not(feature = "target_web"))]
const TARGET_WEB: u8 = 0;

pub const TARGET_FEATURES: u8 = TARGET_STD | TARGET_WEB;

#[cfg(feature = "std")]
const RUNTIME_STD: u8 = 0x00000001;

#[cfg(not(feature = "std"))]
const RUNTIME_STD: u8 = 0;

#[cfg(feature = "web")]
const RUNTIME_WEB: u8 = 0b00000010;

#[cfg(not(feature = "web"))]
const RUNTIME_WEB: u8 = 0;

pub const RUNTIME_FEATURES: u8 = RUNTIME_STD | RUNTIME_WEB;

pub fn features_compatible(target_features: u8, runtime_features: u8) -> bool {
    target_features & runtime_features == target_features
}
