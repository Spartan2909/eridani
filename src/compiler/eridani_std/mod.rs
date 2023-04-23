pub const ERIDANI_STD_BASIC: &str = include_str!("eridani_std_basic.eri");

#[cfg(feature = "target_std")]
pub const ERIDANI_STD_FEATURE_STD: &str = include_str!("eridani_std_feature_std.eri");

#[cfg(feature = "target_web")]
pub const ERIDANI_STD_FEATURE_WEB: &str = include_str!("eridani_std_feature_web.eri");

pub const PRELUDE: [&str; 2] = ["index", "print"];
