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
use expr::*;
use rows::RowBlock;
use rows::vector::{ArrayVector, ConstVector, Vector};
use schema::{Column, Schema};
use types::*;

// Unary Expressions
pub struct Not {child: Box<Eval>}
pub struct IsNull {child: Box<Eval>}

// Binary Comparison Expressions
pub struct AndEval<'a> {
  res_ty: DataTy, // TODO - to be replaced by a a singleton instance
  lhs: Box<MapEval>,
  rhs: Box<MapEval>,
  result: ArrayVector<'a>,
  f: Option<fn(&mut Vector, &Vector, &Vector, Option<&[usize]>)>  
}

impl<'a> AndEval<'a> {
  pub fn new(lhs: Box<MapEval>, rhs: Box<MapEval>) -> AndEval<'a> {
    AndEval {
      res_ty: DataTy::new(Ty::Bool),
      lhs: lhs,
      rhs: rhs,
      result: ArrayVector::new(DataTy::new(Ty::Bool)),
      f: None
    }
  }
}

impl<'a> Eval for AndEval<'a> {

  fn bind(&mut self, schema: &Schema) -> Void {    
    try!(self.lhs.bind(schema));
    try!(self.rhs.bind(schema));

    assert_eq!(Ty::Bool, self.lhs.data_ty().ty());
    assert_eq!(Ty::Bool, self.rhs.data_ty().ty());

    self.f = Some(get_and_primitive(self.lhs.is_const(),self.rhs.is_const()));

    void_ok()
  }  
  
  fn is_const(&self) -> bool { false }
}

impl<'a> HasDataTy for AndEval<'a> {
  fn data_ty(&self) -> &DataTy {
    &self.res_ty
  }
}

pub struct Or {lhs: Box<Eval>, rhs: Box<Eval>}

pub struct CompEval<'a> {
  op: CompOp,
  res_ty: DataTy,
  lhs: Box<MapEval>,
  rhs: Box<MapEval>,
  result: ArrayVector<'a>,

  f: Option<fn(&mut Vector, &Vector, &Vector, Option<&[usize]>)>
}

impl<'a> CompEval<'a> {
  pub fn new(op: CompOp, lhs: Box<MapEval>, rhs: Box<MapEval>) 
      -> CompEval<'a> {

    CompEval {
      op: op,
      res_ty: DataTy::new(Ty::Bool),
      lhs: lhs,
      rhs: rhs,
      result: ArrayVector::new(DataTy::new(Ty::Bool)),
      f: None
    }
  }
}

impl<'a> Eval for CompEval<'a> {

  fn bind(&mut self, schema: &Schema) -> Void {    
    try!(self.lhs.bind(schema));
    try!(self.rhs.bind(schema));

    assert_eq!(self.lhs.data_ty(), self.rhs.data_ty());

    self.f = Some(get_comp_primitive(
                    &self.op,
                    &self.lhs.data_ty(), self.lhs.is_const(),
                    &self.rhs.data_ty(), self.rhs.is_const()));

    void_ok()
  }  
  
  fn is_const(&self) -> bool { false }
}

impl<'a> HasDataTy for CompEval<'a> {
  fn data_ty(&self) -> &DataTy {
    &self.res_ty
  }
}

impl<'a> MapEval for CompEval<'a> {
  fn eval<'r>(&'r mut self, r: &'r RowBlock) -> &'r Vector {
    self.f.unwrap()(&mut self.result as &mut Vector, 
                    self.lhs.eval(r), 
                    self.rhs.eval(r), 
                    None);
    &self.result
  }
}

// Binary Arithmetic Evalessions
pub struct ArithmMapEval<'a> {
  pub op: ArithmOp, 
  pub data_ty: Option<DataTy>, 
  pub lhs: Box<MapEval>,
  pub rhs: Box<MapEval>,
  pub result: Option<ArrayVector<'a>>,
  pub f: Option<fn(&mut Vector, &Vector, &Vector, Option<&[usize]>)>  
}

impl<'a> ArithmMapEval<'a> {
  pub fn new(op: ArithmOp, lhs: Box<MapEval>, rhs: Box<MapEval>) 
      -> ArithmMapEval<'a> {

    ArithmMapEval {
      op: op,
      data_ty: None, // initialized by bind(),
      lhs: lhs,
      rhs: rhs,
      result: None,
      f: None
    }
  }
}

impl<'a> Eval for ArithmMapEval<'a> {

  fn bind(&mut self, schema: &Schema) -> Void {
    self.data_ty = Some(result_data_ty(self.lhs.data_ty(), self.rhs.data_ty()));
    
    try!(self.lhs.bind(schema));
    try!(self.rhs.bind(schema));

    self.result = Some(ArrayVector::new(self.data_ty.unwrap().clone()));

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
    self.f.unwrap()(self.result.as_mut().unwrap(), 
                    self.lhs.eval(r), 
                    self.rhs.eval(r), 
                    None);
    self.result.as_ref().unwrap()
  }
}

// String operators or pattern matching predicates
pub struct Concatenate {lhs: Box<Eval>, rhs: Box<Eval>}
pub struct Like {pattern: String, child: Box<Eval>}
pub struct SimilarTo {pattern: String, child: Box<Eval>}
pub struct RegexMatch {pattern: String, child: Box<Eval>}

pub struct Between {pred: Box<Eval>, begin: Box<Eval>, end: Box<Eval>}
pub struct In<'a> {pred: Box<Eval>, row: Box<Row<'a>>}

pub struct Row<'a> {
  values: ArrayVector<'a>
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

pub struct ConstEval {
  v: ConstVector
}

impl ConstEval {
  fn new(datum: &Datum) -> ConstEval {
    ConstEval {
      v: ConstVector::new(datum.clone())
    }
  }
}

impl HasDataTy for ConstEval {
  fn data_ty(&self) -> &DataTy {
    &self.v.data_ty()
  }
}

impl Eval for ConstEval {
  
  fn bind(&mut self, schema: &Schema) -> Void {    
    void_ok()
  }  
  
  fn is_const(&self) -> bool { true }
}

impl MapEval for ConstEval {
  fn eval<'r>(&'r mut self, r: &'r RowBlock) -> &'r Vector {
    &self.v
  }
}

pub fn compile_map_eval(expr: &Expr) -> Box<MapEval> {
  let mut compiler = Box::new(MapInterpreterCompiler::new());
  walk_expr(&mut *compiler, expr);

  compiler.tree.unwrap()
}

pub struct MapInterpreterCompiler {
  tree: Option<Box<MapEval>>,
  node_num: u32
}

impl MapInterpreterCompiler {
  fn new() -> MapInterpreterCompiler {
    InterpreterCompiler {
      tree: None,
      node_num: 0
    }
  }  

  fn walk_and_take_bin_expr(&mut self, lhs: &Expr, rhs: &Expr) -> 
      (Box<MapEval>, Box<MapEval>) {

    walk_expr(self, lhs);
    let lhs = self.tree.take();
    walk_expr(self, rhs);
    let rhs = self.tree.take();

    (lhs.unwrap(), rhs.unwrap())
  }
}

impl<'v> Visitor<'v> for InterpreterCompiler {
  fn visit_comp(&mut self, op: &CompOp, lhs: &'v Expr, rhs: &'v Expr) {    
    let childs = self.walk_and_take_bin_expr(lhs, rhs);

    self.tree = Some(
      Box::new(CompEval::new(*op, childs.0, childs.1))
    );
  }

  fn visit_arithm(&mut self, op: &ArithmOp, lhs: &'v Expr, rhs: &'v Expr) {    
    let childs = self.walk_and_take_bin_expr(lhs, rhs);

    self.tree = Some(
      Box::new(ArithmMapEval::new(*op, childs.0, childs.1))
    );
  }

  fn visit_field(&mut self, c: &'v Column) {
    self.tree = Some(Box::new(Field::new(c)));
  } 

  fn visit_const(&mut self, d: &'v Datum) {
    self.tree = Some(Box::new(ConstEval::new(d)));
  }
}

fn get_and_primitive(lhs_const: bool, rhs_const: bool) -> 
    fn(&mut Vector, &Vector, &Vector, Option<&[usize]>) {

  match (lhs_const, rhs_const) {
    (true, false) => map_and_cv,
    (false, true) => map_and_vc,
    (false, false) => map_and_vv,
    _ => panic!("unsupported const vs. const operation")
  }
}

fn get_arithm_prim(op: &ArithmOp, 
                   res_ty: &DataTy, 
                   lhs_dty: &DataTy, lhs_vec: bool,
                   rhs_dty: &DataTy, rhs_vec: bool) 
    -> fn(&mut Vector, &Vector, &Vector, Option<&[usize]>) {

  assert_eq!(lhs_dty, rhs_dty);

  match lhs_dty.ty() {
    Ty::Int2      => get_arithm_vec_or_const::<INT2_T>     (op, lhs_vec, rhs_vec),
    Ty::Int4      => get_arithm_vec_or_const::<INT4_T>     (op, lhs_vec, rhs_vec),
    Ty::Int8      => get_arithm_vec_or_const::<INT8_T>     (op, lhs_vec, rhs_vec),
    Ty::Float4    => get_arithm_vec_or_const::<FLOAT4_T>   (op, lhs_vec, rhs_vec),
    Ty::Float8    => get_arithm_vec_or_const::<FLOAT8_T>   (op, lhs_vec, rhs_vec),
    Ty::Time      => get_arithm_vec_or_const::<TIME_T>     (op, lhs_vec, rhs_vec),
    Ty::Date      => get_arithm_vec_or_const::<DATE_T>     (op, lhs_vec, rhs_vec),
    Ty::Timestamp => get_arithm_vec_or_const::<TIMESTAMP_T>(op, lhs_vec, rhs_vec),
    _ => panic!("unsupported data type")
  }
}

fn get_arithm_vec_or_const<T>(op: &ArithmOp, lhs_const: bool, rhs_const: bool)
  -> fn(&mut Vector, &Vector, &Vector, Option<&[usize]>) 
  where T : Copy + Display + ops::Add<T, Output=T> + ops::Sub<T, Output=T> +
            ops::Mul<T, Output=T> + ops::Div<T, Output=T> + ops::Rem<T, Output=T> {

  match *op {
    
    ArithmOp::Plus => {
      match (lhs_const, rhs_const) {
        (true, false) => map_plus_cv::<T>,
        (false, true) => map_plus_vc::<T>,
        (false, false) => map_plus_vv::<T>,   
        _ => panic!("plus operation between const and const is not supported yet.")
      }
    },

    ArithmOp::Sub => {
      match (lhs_const, rhs_const) {
        (true, false) => map_sub_cv::<T>,
        (false, true) => map_sub_vc::<T>,    
        (false, false) => map_sub_vv::<T>,    
        _ => panic!("sub operation between const and const is not supported yet.")
      }
    },

    ArithmOp::Mul => {
      match (lhs_const, rhs_const) {
        (true, false) => map_mul_cv::<T>,
        (false, true) => map_mul_vc::<T>,    
        (false, false) => map_mul_vv::<T>,    
        _ => panic!("mul operation between const and const is not supported yet.")
      }
    },

    ArithmOp::Div => {
      match (lhs_const, rhs_const) {
        (true, false) => map_div_cv::<T>,
        (false, true) => map_div_vc::<T>,    
        (false, false) => map_div_vv::<T>,    
        _ => panic!("div operation between const and const is not supported yet.")
      }
    },

    ArithmOp::Rem => {
      match (lhs_const, rhs_const) {
        (true, false) => map_rem_cv::<T>,
        (false, true) => map_rem_vc::<T>,
        (false, false) => map_rem_vv::<T>,    
        _ => panic!("rem operation between const and const is not supported yet.")
      }
    }
  }
}

fn get_comp_primitive(op: &CompOp,                       
                      lhs_dty: &DataTy, lhs_const: bool,
                      rhs_dty: &DataTy, rhs_const: bool) 
    -> fn(&mut Vector, &Vector, &Vector, Option<&[usize]>) {

  assert_eq!(lhs_dty, rhs_dty);

  match (lhs_dty.ty) {
    Ty::Int2      => get_comp_vec_or_const::<INT2_T>     (op, lhs_const, rhs_const),
    Ty::Int4      => get_comp_vec_or_const::<INT4_T>     (op, lhs_const, rhs_const),
    Ty::Int8      => get_comp_vec_or_const::<INT8_T>     (op, lhs_const, rhs_const),
    Ty::Float4    => get_comp_vec_or_const::<FLOAT4_T>   (op, lhs_const, rhs_const),
    Ty::Float8    => get_comp_vec_or_const::<FLOAT8_T>   (op, lhs_const, rhs_const),
    Ty::Time      => get_comp_vec_or_const::<TIME_T>     (op, lhs_const, rhs_const),
    Ty::Date      => get_comp_vec_or_const::<DATE_T>     (op, lhs_const, rhs_const),
    Ty::Timestamp => get_comp_vec_or_const::<TIMESTAMP_T>(op, lhs_const, rhs_const),
    Ty::Text      => get_comp_vec_or_const::<TEXT_T>     (op, lhs_const, rhs_const),
    _             => panic!("unsupported data type")
  }
}

fn get_comp_vec_or_const<T>(op: &CompOp, lhs_const: bool, rhs_const: bool) -> 
    fn(&mut Vector, &Vector, &Vector, Option<&[usize]>)
    where T: Copy + Display + PartialEq + PartialOrd {
  
  match *op {
    CompOp::Eq => {
      match (lhs_const, rhs_const) {
        (true, false)  => map_eq_cv::<T>,
        (false, true)  => map_eq_vc::<T>,
        (false, false) => map_eq_vv::<T>,
        _ => panic!("binary operation between const and const is not supported yet.")
      }
    },

    CompOp::Ne => {
      match (lhs_const, rhs_const) {
        (true, false)  => map_ne_cv::<T>,
        (false, true)  => map_ne_vc::<T>,
        (false, false) => map_ne_vv::<T>,
        _ => panic!("binary operation between const and const is not supported yet.")
      }
    },

    CompOp::Lt => {
      match (lhs_const, rhs_const) {
        (true, false)  => map_lt_cv::<T>,
        (false, true)  => map_lt_vc::<T>,
        (false, false) => map_lt_vv::<T>,
        _ => panic!("binary operation between const and const is not supported yet.")
      }
    },

    CompOp::Le => {
      match (lhs_const, rhs_const) {
        (true, false)  => map_le_cv::<T>,
        (false, true)  => map_le_vc::<T>,
        (false, false) => map_le_vv::<T>,
        _ => panic!("binary operation between const and const is not supported yet.")
      }
    },

    CompOp::Gt => {
      match (lhs_const, rhs_const) {
        (true, false)  => map_gt_cv::<T>,
        (false, true)  => map_gt_vc::<T>,
        (false, false) => map_gt_vv::<T>,
        _ => panic!("binary operation between const and const is not supported yet.")
      }
    },

    CompOp::Ge => {
      match (lhs_const, rhs_const) {
        (true, false)  => map_ge_cv::<T>,
        (false, true)  => map_ge_vc::<T>,
        (false, false) => map_ge_vv::<T>,
        _ => panic!("binary operation between const and const is not supported yet.")
      }
    }
  }
}