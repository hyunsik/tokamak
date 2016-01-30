#![feature(libc)]
extern crate libc;
extern crate parser;
extern crate readline;

use std::rc::Rc;
use readline::*;
use parser::lexer;
use parser::parser as p;

// Keep
struct ReplContext;

pub fn main() {
  unsafe {
    let mut ast = Vec::new();

    loop {
      let line = readline(from_str("\x1b[33mtkm> \x1b[0m"));
      if line.is_null() {
        break;
      }

      let tokens = lexer::tokenize(to_str(line));
      let parsed = p::parse(&tokens[..], &ast[..]);

      match parsed {
        Ok(r) => {
          match r.1.len() {
            0 => {
              println!("{:?}, {:?}", r.0, r.1);
              add_history(line);
            }
            _ => {}
          }
        }
        Err(msg) => {println!("{}", msg)}
      }
    }
  }
}