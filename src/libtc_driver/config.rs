use syntax::errors::{ColorConfig, Handler};

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