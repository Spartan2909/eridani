use std::{env, fs, path};

use rustc_version::{version_meta, Channel};

fn main() {
    let mut out_dir = path::PathBuf::from(env::var("OUT_DIR").unwrap());
    out_dir.push("eridani_version_data.rs");

    let version_data = version_meta().unwrap();
    let rust_major = version_data.semver.major;
    let rust_minor = version_data.semver.minor;
    let rust_patch = version_data.semver.patch;
    let pre = version_data.semver.pre.as_str();
    let channel = match version_data.channel {
        Channel::Dev => "RustChannel::Dev",
        Channel::Nightly => "RustChannel::Nightly",
        Channel::Beta => "RustChannel::Beta",
        Channel::Stable => "RustChannel::Stable",
    };

    let eridani_major: u64 = env::var("CARGO_PKG_VERSION_MAJOR")
        .unwrap()
        .parse()
        .unwrap();
    let eridani_minor: u64 = env::var("CARGO_PKG_VERSION_MINOR")
        .unwrap()
        .parse()
        .unwrap();
    let eridani_patch: u64 = env::var("CARGO_PKG_VERSION_PATCH")
        .unwrap()
        .parse()
        .unwrap();

    let contents = format!(
        "
        const RUST_VERSION_DATA: RustVersionData = RustVersionData {{
            major: {rust_major},
            minor: {rust_minor},
            patch: {rust_patch},
            pre: \"{pre}\",
            channel: {channel},
        }};

        pub const VERSION_DATA: VersionData = VersionData {{
            rust_version: RUST_VERSION_DATA,
            major: {eridani_major},
            minor: {eridani_minor},
            patch: {eridani_patch},
        }};
    "
    );
    let _ = fs::write(out_dir, contents);
}
