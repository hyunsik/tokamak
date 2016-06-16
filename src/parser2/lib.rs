#![feature(const_fn)]
#![feature(filling_drop)]
#![feature(unicode)]
#![feature(question_mark)]
#![feature(str_escape)]

#[macro_use] extern crate bitflags;
extern crate env_logger;
#[macro_use ] extern crate itertools;
#[macro_use] extern crate log;
extern crate term;

pub mod abi;
pub mod ast;
pub mod ast_printer;
pub mod attr;
pub mod codemap;
pub mod comments;
pub mod error_handler;
pub mod interner;
pub mod lexer;
pub mod parser;
pub mod precedence;
pub mod ptr;
pub mod token;
pub mod unicode_chars;


