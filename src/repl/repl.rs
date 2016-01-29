#![feature(libc)]
extern crate libc;
extern crate parser;
extern crate readline;

use std::rc::Rc;
use readline::*;
use parser::lexer;
use parser::parser as p;

pub fn main() {
  unsafe {
    loop {
      let line = readline(from_str("\x1b[33mtkm> \x1b[0m"));
      if line.is_null() {
        break;
      }

      let tokens = lexer::tokenize(to_str(line));
      let ast = Vec::new();
      let parsed = p::parse(&tokens[..], &ast[..]);

      for x in parsed.ok().unwrap().0 {
        println!("{:?}", x);
      }

      add_history(line);
    }
  }
}