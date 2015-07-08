//!
//! Interpreter Compiler for Expressions
//!

use std::boxed::Box;
use common::schema::Column;
use eval::Eval;
use expr::{Expr, Visitor};
use std::marker;

pub struct InterpreterCompiler {
  last: Box<Eval>,
  //_marker: marker::PhantomData<&'a ()>
}

impl<'v> Visitor<'v> for InterpreterCompiler {
  fn visit_field(&mut self, c: &'v Column) {
    println!("column: {}", c.name); 
  } 
}