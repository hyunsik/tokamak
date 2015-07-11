//!
//! Interpreter Compiler for Expressions
//!

use std::boxed::Box;
use std::ops;

use common::constant::VECTOR_SIZE;
use common::err::{Error, TResult, Void, void_ok};
use eval::{Eval, MapEval};
use eval::primitives::*;
use expr::{Datum, Expr, Visitor};
use rows::RowBlock;
use rows::vector::Vector;
use schema::{Column, Schema};
use types::{DataTy, HasDataTy, HasTy, result_data_ty, Ty};

// Unary Expressions
pub struct Not {child: Box<Eval>}
pub struct IsNull {child: Box<Eval>}

// Binary Comparison Expressions
pub struct And {lhs: Box<Eval>, rhs: Box<Eval>}
pub struct Or {lhs: Box<Eval>, rhs: Box<Eval>}
pub struct Equal {lhs: Box<Eval>, rhs: Box<Eval>}
pub struct NotEqual {lhs: Box<Eval>, rhs: Box<Eval>}
pub struct LessThan {lhs: Box<Eval>, rhs: Box<Eval>}
pub struct LessThanOrEqual {lhs: Box<Eval>, rhs: Box<Eval>}
pub struct GreaterThan {lhs: Box<Eval>, rhs: Box<Eval>}
pub struct GreaterThanOrEqual {lhs: Box<Eval>, rhs: Box<Eval>}

// Binary Arithmetic Evalessions
pub struct Plus<'r> {data_ty: DataTy, pub lhs: Box<MapEval<'r>>, pub rhs: Box<MapEval<'r>>}
pub struct Minus {lhs: Box<Eval>, rhs: Box<Eval>}
pub struct Multiply {lhs: Box<Eval>, rhs: Box<Eval>}
pub struct Divide {lhs: Box<Eval>, rhs: Box<Eval>}
pub struct Modular {lhs: Box<Eval>, rhs: Box<Eval>}

// String operators or pattern matching predicates
pub struct Concatenate {lhs: Box<Eval>, rhs: Box<Eval>}
pub struct Like {pattern: String, child: Box<Eval>}
pub struct SimilarTo {pattern: String, child: Box<Eval>}
pub struct RegexMatch {pattern: String, child: Box<Eval>}

pub struct Between {pred: Box<Eval>, begin: Box<Eval>, end: Box<Eval>}
pub struct In {pred: Box<Eval>, row: Box<Row>}

pub struct Row {values: Vec<Box<Eval>>}
pub struct Field {column: Column, field_id: usize}
pub struct Const {datum: Datum, res_type: DataTy}


impl<'r> Eval for Plus<'r> {
  fn bind(&mut self, schema: &Schema) -> Void {
    self.data_ty = result_data_ty(self.lhs.data_ty(), self.rhs.data_ty());
    void_ok()
  }  
  
  fn is_const(&self) -> bool { false }
}

impl<'r> MapEval<'r> for Plus<'r> {
  fn eval(&self, r: &'r RowBlock<'r>) -> &'r Vector {
    let l: &Vector = self.lhs.eval(r);
    let r: &Vector = self.rhs.eval(r);

    let f: Option<fn(&mut Vector, &Vector, &Vector)> = Some(if true {
      map_plus_vv::<i32>
    } else {
      map_plus_vv::<i64>
    });

    l
  }
}

impl<'r> HasDataTy for Plus<'r> {
  #[inline]
  fn data_ty(&self) -> &DataTy {
    &self.data_ty
  }
}

impl Field {
  pub fn new(column: &Column) -> Field {
    Field {
      column: column.clone(),
      field_id: 0
    }
  }
}

impl HasDataTy for Field {
  fn data_ty(&self) -> &DataTy {
    &self.column.data_ty
  }
}

impl Eval for Field {
  
  fn bind(&mut self, schema: &Schema) -> Void {
      
    match schema.column_id(&self.column.name) {
        
     Some(id) => {
      self.field_id = id;
      void_ok()
     },
     
     None => Err(Error::UndefinedColumn)
    }    
  }  
  
  fn is_const(&self) -> bool { false }  
}

impl<'r> MapEval<'r> for Field {
  fn eval(&self, r: &'r RowBlock<'r>) -> &'r Vector {
    r.vector(self.field_id)
  }
}

impl Const {
  fn new(datum: &Datum) -> Const {
    Const {
        datum: datum.clone(), 
        res_type: DataTy::new(datum.ty())
    }
  }
}

impl HasDataTy for Const {
  fn data_ty(&self) -> &DataTy {
    &self.res_type
  }
}

impl Eval for Const {
  
  fn bind(&mut self, schema: &Schema) -> Void {
    self.res_type = DataTy::new(self.datum.ty());
    void_ok()
  }  
  
  fn is_const(&self) -> bool { true }
}

pub struct InterpreterCompiler {
  tree: Option<Box<Eval>>,
  err: Option<Error>,
  node_num: u32
}

impl<'v> Visitor<'v> for InterpreterCompiler {
  fn visit_field(&mut self, c: &'v Column) {
    println!("column: {}", c.name); 
  } 
}