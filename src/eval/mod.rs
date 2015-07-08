//!
//! Expression Evaluator Trait and Implementation
//!

use common::err::{Error, TResult, Void, void_ok};
use rows::RowBlock;
use rows::vector::Vector1;
use types::{DataType, Ty, HasDataTy, HasTy};
use schema::{Column, Schema};
use expr::{Datum, Expr};

/// Common Trait of All Expression Evaluators  
pub trait Eval : HasDataTy {
  fn bind(&mut self, schema: &Schema) -> Void;

  fn is_const(&self) -> bool;
}

/// Map Expression Evaluator Trait
pub trait MapEval : Eval {
  fn eval(&self, RowBlock) -> TResult<&Vector1>;
}

/// Filter Expression Evaluator Trait
pub trait FilterEval : Eval {
  fn filter(&self, RowBlock) -> Void;
}

/// Map Expression Evaluator Compiler
pub trait MapCompiler<'a> {
  fn compile(expr: &'a Expr) -> TResult<Box<MapEval>>;
}

/// Filter Expression Evaluator Compiler
pub trait FilterCompiler<'a> {
  fn compile(expr: &'a Expr) -> TResult<Box<FilterEval>>;   
}

/// Interpreter Compiler for Expressions
pub mod interpreter;