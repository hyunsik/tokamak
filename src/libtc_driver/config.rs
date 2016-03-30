use syntax::errors::{ColorConfig, Handler};

use getopts;
use std::path::PathBuf;

pub struct Config;

pub struct Options;

pub enum Input {
  PathBuf
}

pub fn host_triple() -> &'static str {
    // Get the host triple out of the build environment. This ensures that our
    // idea of the host triple is the same as for the set of libraries we've
    // actually built.  We can't just take LLVM's host triple because they
    // normalize all ix86 architectures to i386.
    //
    // Instead of grabbing the host triple (for the current host), we grab (at
    // compile time) the target triple that this rustc is built with and
    // calling that (at runtime) the host triple.
    (option_env!("CFG_COMPILER_HOST_TRIPLE")).
        expect("CFG_COMPILER_HOST_TRIPLE")
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ErrorOutputType {
    HumanReadable(ColorConfig),
    Json,
}

impl Default for ErrorOutputType {
    fn default() -> ErrorOutputType {
        ErrorOutputType::HumanReadable(ColorConfig::Auto)
    }
}

/// Returns the "short" subset of the rustc command line options,
/// including metadata for each option, such as whether the option is
/// part of the stable long-term interface for rustc.
pub fn compiler_short_optgroups() -> getopts::Options {
  getopts::Options::new()
}

/// Returns all rustc command line options, including metadata for
/// each option, such as whether the option is part of the stable
/// long-term interface for rustc.
pub fn compiler_optgroups() -> getopts::Options {
  compiler_short_optgroups()
}