#![feature(set_stdio)]
extern crate fnv;
#[macro_use] extern crate log;

extern crate flang_common as common;
extern crate flang_errors as errors;
extern crate parser as parser;

mod incremental;
pub mod metadata;

pub use incremental::{IncrCompilerAction, IncrCompiler};


use std::path::PathBuf;

pub enum Input {
    /// Load source from file
    File(PathBuf),
    Str {
        /// String that is shown in place of a filename
        name: String,
        /// Anonymous source string
        input: String,
    },
}

impl Input {
    pub fn filestem(&self) -> String {
        match *self {
            Input::File(ref ifile) => ifile.file_stem().unwrap()
                                           .to_str().unwrap().to_string(),
            Input::Str { .. } => "rust_out".to_string(),
        }
    }
}