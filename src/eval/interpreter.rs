//!
//! Interpreter Compiler for Expressions
//!

use std::boxed::Box;
use std::ops;
use std::fmt::Display;

use common::constant::VECTOR_SIZE;
use common::err::{Error, TResult, Void, void_ok};
use eval::{Eval, MapEval};
use eval::primitives::*;
use expr::{ArithmOp, Datum, Expr, Visitor};
use rows::RowBlock;
use rows::vector::{Vector, ArrayVector};
use schema::{Column, Schema};
use types::*;

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
pub struct ArithmMapEval<'a> {
  pub op: ArithmOp, 
  pub data_ty: Option<DataTy>, 
  pub lhs: Box<MapEval>,
  pub rhs: Box<MapEval>,
  pub result: ArrayVector<'a>,
  pub f: Option<fn(&mut Vector, &Vector, &Vector, Option<&[usize]>)>  
}

// String operators or pattern matching predicates
pub struct Concatenate {lhs: Box<Eval>, rhs: Box<Eval>}
pub struct Like {pattern: String, child: Box<Eval>}
pub struct SimilarTo {pattern: String, child: Box<Eval>}
pub struct RegexMatch {pattern: String, child: Box<Eval>}

pub struct Between {pred: Box<Eval>, begin: Box<Eval>, end: Box<Eval>}
pub struct In {pred: Box<Eval>, row: Box<Row>}

pub struct Row {values: Vec<Box<Eval>>}

impl<'a> Eval for ArithmMapEval<'a> {

  fn bind(&mut self, schema: &Schema) -> Void {
    self.data_ty = Some(result_data_ty(self.lhs.data_ty(), self.rhs.data_ty()));
    try!(self.lhs.bind(schema));
    try!(self.rhs.bind(schema));

    self.f = Some(get_arithm_prim(&self.op,
                                  self.data_ty.as_ref().unwrap(),
                                  &self.lhs.data_ty(), self.lhs.is_const(),
                                  &self.rhs.data_ty(), self.rhs.is_const()));

    void_ok()
  }  
  
  fn is_const(&self) -> bool { false }
}

impl<'a> HasDataTy for ArithmMapEval<'a> {
  fn data_ty(&self) -> &DataTy {
    self.data_ty.as_ref().unwrap()
  }
}

impl<'a> MapEval for ArithmMapEval<'a> {
  fn eval<'r>(&'r mut self, r: &'r RowBlock) -> &'r Vector {
    self.f.unwrap()(&mut self.result, 
                    self.lhs.eval(r), 
                    self.rhs.eval(r), 
                    None);
    &self.result
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

pub struct Field {column: Column, field_id: usize}

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

impl MapEval for Field {
  fn eval<'r>(&'r mut self, r: &'r RowBlock) -> &'r Vector {
    r.vector(self.field_id)
  }
}

pub struct Const {datum: Datum, res_type: DataTy}

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

fn get_arithm_prim(op: &ArithmOp, 
                   res_ty: &DataTy, 
                   lhs_dty: &DataTy, lhs_vec: bool,
                   rhs_dty: &DataTy, rhs_vec: bool) 
    -> fn(&mut Vector, &Vector, &Vector, Option<&[usize]>) {

  assert_eq!(lhs_dty, rhs_dty);

  match *op {

    ArithmOp::Plus => {
      match lhs_dty.ty() {
        Ty::Int2      => get_arithm_plus_vec_or_const::<INT2_T>(lhs_vec, rhs_vec),
        Ty::Int4      => get_arithm_plus_vec_or_const::<INT4_T>(lhs_vec, rhs_vec),
        Ty::Int8      => get_arithm_plus_vec_or_const::<INT8_T>(lhs_vec, rhs_vec),
        Ty::Float4    => get_arithm_plus_vec_or_const::<FLOAT4_T>(lhs_vec, rhs_vec),
        Ty::Float8    => get_arithm_plus_vec_or_const::<FLOAT8_T>(lhs_vec, rhs_vec),
        Ty::Time      => get_arithm_plus_vec_or_const::<TIME_T>(lhs_vec, rhs_vec),
        Ty::Date      => get_arithm_plus_vec_or_const::<DATE_T>(lhs_vec, rhs_vec),
        Ty::Timestamp => get_arithm_plus_vec_or_const::<TIMESTAMP_T>(lhs_vec, rhs_vec),
        _ => panic!("unsupported data type")
      }
    },

    ArithmOp::Sub => {
      match lhs_dty.ty() {
        Ty::Int2      => get_arithm_sub_vec_or_const::<INT2_T>(lhs_vec, rhs_vec),
        Ty::Int4      => get_arithm_sub_vec_or_const::<INT4_T>(lhs_vec, rhs_vec),
        Ty::Int8      => get_arithm_sub_vec_or_const::<INT8_T>(lhs_vec, rhs_vec),
        Ty::Float4    => get_arithm_sub_vec_or_const::<FLOAT4_T>(lhs_vec, rhs_vec),
        Ty::Float8    => get_arithm_sub_vec_or_const::<FLOAT8_T>(lhs_vec, rhs_vec),
        Ty::Time      => get_arithm_sub_vec_or_const::<TIME_T>(lhs_vec, rhs_vec),
        Ty::Date      => get_arithm_sub_vec_or_const::<DATE_T>(lhs_vec, rhs_vec),
        Ty::Timestamp => get_arithm_sub_vec_or_const::<TIMESTAMP_T>(lhs_vec, rhs_vec),
        _ => panic!("unsupported data type")
      }
    },

    ArithmOp::Mul => {
      match lhs_dty.ty() {
        Ty::Int2      => get_arithm_mul_vec_or_const::<INT2_T>(lhs_vec, rhs_vec),
        Ty::Int4      => get_arithm_mul_vec_or_const::<INT4_T>(lhs_vec, rhs_vec),
        Ty::Int8      => get_arithm_mul_vec_or_const::<INT8_T>(lhs_vec, rhs_vec),
        Ty::Float4    => get_arithm_mul_vec_or_const::<FLOAT4_T>(lhs_vec, rhs_vec),
        Ty::Float8    => get_arithm_mul_vec_or_const::<FLOAT8_T>(lhs_vec, rhs_vec),
        Ty::Time      => get_arithm_mul_vec_or_const::<TIME_T>(lhs_vec, rhs_vec),
        Ty::Date      => get_arithm_mul_vec_or_const::<DATE_T>(lhs_vec, rhs_vec),
        Ty::Timestamp => get_arithm_mul_vec_or_const::<TIMESTAMP_T>(lhs_vec, rhs_vec),
        _ => panic!("unsupported data type")
      }
    },

    ArithmOp::Div => {
      match lhs_dty.ty() {
        Ty::Int2      => get_arithm_div_vec_or_const::<INT2_T>(lhs_vec, rhs_vec),
        Ty::Int4      => get_arithm_div_vec_or_const::<INT4_T>(lhs_vec, rhs_vec),
        Ty::Int8      => get_arithm_div_vec_or_const::<INT8_T>(lhs_vec, rhs_vec),
        Ty::Float4    => get_arithm_div_vec_or_const::<FLOAT4_T>(lhs_vec, rhs_vec),
        Ty::Float8    => get_arithm_div_vec_or_const::<FLOAT8_T>(lhs_vec, rhs_vec),
        Ty::Time      => get_arithm_div_vec_or_const::<TIME_T>(lhs_vec, rhs_vec),
        Ty::Date      => get_arithm_div_vec_or_const::<DATE_T>(lhs_vec, rhs_vec),
        Ty::Timestamp => get_arithm_div_vec_or_const::<TIMESTAMP_T>(lhs_vec, rhs_vec),
        _ => panic!("unsupported data type")
      }
    },

    ArithmOp::Rem => {
      match lhs_dty.ty() {
        Ty::Int2      => get_arithm_rem_vec_or_const::<INT2_T>(lhs_vec, rhs_vec),
        Ty::Int4      => get_arithm_rem_vec_or_const::<INT4_T>(lhs_vec, rhs_vec),
        Ty::Int8      => get_arithm_rem_vec_or_const::<INT8_T>(lhs_vec, rhs_vec),
        Ty::Float4    => get_arithm_rem_vec_or_const::<FLOAT4_T>(lhs_vec, rhs_vec),
        Ty::Float8    => get_arithm_rem_vec_or_const::<FLOAT8_T>(lhs_vec, rhs_vec),
        Ty::Time      => get_arithm_rem_vec_or_const::<TIME_T>(lhs_vec, rhs_vec),
        Ty::Date      => get_arithm_rem_vec_or_const::<DATE_T>(lhs_vec, rhs_vec),
        Ty::Timestamp => get_arithm_rem_vec_or_const::<TIMESTAMP_T>(lhs_vec, rhs_vec),
        _ => panic!("unsupported data type")
      }
    }
  }  
}

fn get_arithm_plus_vec_or_const<T>(lhs_const: bool, rhs_const: bool)     
    -> fn(&mut Vector, &Vector, &Vector, Option<&[usize]>) 
    where T : Copy + Display + ops::Add<T, Output=T> {
  
  match (lhs_const, rhs_const) {
    (true, false) => map_plus_cv::<T>,
    (false, true) => map_plus_vc::<T>,
    (false, false) => map_plus_vv::<T>,   
    _ => panic!("plus operation between const and const is not supported yet.")
  }
}

fn get_arithm_sub_vec_or_const<T>(lhs_const: bool, rhs_const: bool)     
    -> fn(&mut Vector, &Vector, &Vector, Option<&[usize]>) 
    where T : Copy + Display + ops::Sub<T, Output=T> {
  
  match (lhs_const, rhs_const) {
    (true, false) => map_sub_cv::<T>,
    (false, true) => map_sub_vc::<T>,    
    (false, false) => map_sub_vv::<T>,    
    _ => panic!("sub operation between const and const is not supported yet.")
  }
}

fn get_arithm_mul_vec_or_const<T>(lhs_const: bool, rhs_const: bool)     
    -> fn(&mut Vector, &Vector, &Vector, Option<&[usize]>) 
    where T : Copy + Display + ops::Mul<T, Output=T> {
  
  match (lhs_const, rhs_const) {
    (true, false) => map_mul_cv::<T>,
    (false, true) => map_mul_vc::<T>,    
    (false, false) => map_mul_vv::<T>,    
    _ => panic!("mul operation between const and const is not supported yet.")
  }
}

fn get_arithm_div_vec_or_const<T>(lhs_const: bool, rhs_const: bool)     
    -> fn(&mut Vector, &Vector, &Vector, Option<&[usize]>) 
    where T : Copy + Display + ops::Div<T, Output=T> {
  
  match (lhs_const, rhs_const) {
    (true, false) => map_div_cv::<T>,
    (false, true) => map_div_vc::<T>,    
    (false, false) => map_div_vv::<T>,    
    _ => panic!("div operation between const and const is not supported yet.")
  }
}

fn get_arithm_rem_vec_or_const<T>(lhs_const: bool, rhs_const: bool)     
    -> fn(&mut Vector, &Vector, &Vector, Option<&[usize]>) 
    where T : Copy + Display + ops::Rem<T, Output=T> {
  
  match (lhs_const, rhs_const) {
    (true, false) => map_rem_cv::<T>,
    (false, true) => map_rem_vc::<T>,
    (false, false) => map_rem_vv::<T>,    
    _ => panic!("rem operation between const and const is not supported yet.")
  }
}