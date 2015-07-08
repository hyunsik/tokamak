use std::boxed::Box;
use eval::Eval;
use expr::{Expr, Visitor};
use std::marker;

pub struct InterpreterCompiler {
  last: Box<Eval>,
  //_marker: marker::PhantomData<&'a ()>
}

impl<'a> Visitor<'a> for InterpreterCompiler {

}