#![feature(const_fn)]
#![feature(filling_drop)]
#![feature(unicode)]
#![feature(str_escape)]
#![feature(optin_builtin_traits)]

#[macro_use] extern crate bitflags;
extern crate env_logger;
#[macro_use ] extern crate itertools;
#[macro_use] extern crate log;
extern crate term;
extern crate rustc_unicode as unicode;
extern crate rustc_serialize;

extern crate flang_common as common;
extern crate flang_errors as errors;

pub mod abi;
pub mod ast;
pub mod ast_printer;
pub mod attr;
pub mod codemap;
pub mod comments;
pub mod lexer;
pub mod hygiene;
pub mod parser;
pub mod precedence;
pub mod ptr;
pub mod symbol;
pub mod token;
pub mod tokenstream;
pub mod ttreader;
pub mod unicode_chars;

pub mod util {
    mod thin_vec;
    pub use self::thin_vec::ThinVec;
}
pub mod visitor;


