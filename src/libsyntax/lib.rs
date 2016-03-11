#![feature(unicode)]
#![feature(convert)]
#![feature(libc)]
#![feature(filling_drop)]
#![feature(str_escape)]

#[macro_use] extern crate bitflags;
extern crate libc;
extern crate env_logger;
#[macro_use]
extern crate log;
extern crate rustc_serialize;
extern crate term;

// A variant of 'try!' that panics on an Err. This is used as a crutch on the
// way towards a non-panic!-prone parser. It should be used for fatal parsing
// errors; eventually we plan to convert all code using panictry to just use
// normal try.
// Exported for syntax_ext, not meant for general use.
#[macro_export]
macro_rules! panictry {
    ($e:expr) => ({
        use std::result::Result::{Ok, Err};
        use $crate::errors::FatalError;
        match $e {
            Ok(e) => e,
            Err(mut e) => {
              e.emit();
              panic!(FatalError);
            }
        }
    })
}

pub mod abi;
pub mod ast;
pub mod ast_util;
pub mod attr;
pub mod codemap;
pub mod errors;
pub mod parse;
pub mod ptr;
pub mod str;
pub mod visit;

pub mod diagnostics {
  pub mod registry;
}

pub mod print {
  pub mod pp;
  pub mod pprust;
}

pub mod util {
  pub mod interner;
  pub mod parser;
  pub mod parser_testing;
}