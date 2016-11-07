#![feature(set_stdio)]
extern crate fnv;

extern crate flang_common as common;
extern crate flang_errors as errors;
extern crate parser as parser;

mod incremental;
pub mod metadata;

pub use incremental::{IncrCompilerAction, IncrCompiler};

