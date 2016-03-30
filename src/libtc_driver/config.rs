use syntax::errors::{ColorConfig, Handler};

use getopts;
use std::path::PathBuf;

pub struct Config;

pub struct Options;

pub enum Input {
  PathBuf
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