//!
//! Expression Plan Representation for Tajo Kernel
//!

use std::ops::{Add, Sub, Mul, Div, Rem};

use types::*;
use schema::Column;
use common::P;

/// Datum representation for a single value
#[derive(Clone)]
pub enum Datum {
  Bool(bool),  
  Int1(i8),
  Int2(i16),
  Int4(i32),
  Int8(i64),
  Float4(f32),
  Float8(f64),
  Time(i64),
  Date(i32),
  Timestamp(i64),
  Interval(i64, i32),
  Char(String),
  Text(String),
  Varchar(String),
  Blob(Vec<u8>)
}

impl HasTy for Datum {
  fn data_ty(&self) -> &Ty {
    match *self {
      Datum::Bool(ref x) => &BOOL_TY,
      Datum::Int1(ref x) => &INT1_TY,
      Datum::Int2(ref x) => &INT2_TY,
      Datum::Int4(ref x) => &INT4_TY,
      Datum::Int8(ref x) => &INT8_TY,
      Datum::Float4(ref x) => &FLOAT4_TY,
      Datum::Float8(ref x) => &FLOAT8_TY,
      Datum::Time(ref x) => &TIME_TY,
      Datum::Date(ref x) => &DATE_TY,
      Datum::Timestamp(ref x) => &TIMESTAMP_TY,
      Datum::Interval(ref x,ref y) => &INTERVAL_TY,
      //Datum::Char(ref x) => &CHAR_TY,
      Datum::Text(ref x) => &TEXT_TY,
      //Datum::Varchar(ref x) => &VARCHAR_TY,
      //Datum::Blob(ref x) => &BLOB_TY
      _ => panic!("Unsupported type")
    }
  }
}

/// Function Declaration
#[derive(Clone)]
pub struct FnDecl {
  signature: String
}

/// Aggregation Function Declaration
#[derive(Clone)]
pub struct AggFnDecl {
  signature: String 
}

/// Window Function Declaration
#[derive(Clone)]
pub struct WinFnDecl {
  signature: String  
}

/// Comparison Operator Type
#[derive(Clone, Copy)]
pub enum CompOp {
  Eq,
  Ne,
  Lt,
  Le,
  Gt,
  Ge
}

/// Arithmetic Operator Type
#[derive(Clone, Copy)]
pub enum ArithmOp {
  Plus,
  Sub,
  Mul,
  Div,
  Rem,  
}

/// Expression Element
#[derive(Clone)]
pub struct Expr {
  ty: Ty,
  node: ExprSpec
}

/// Expression Specific Element
#[derive(Clone)]
pub enum ExprSpec {
  // Unary Expressions
  Not(Box<Expr>),
  IsNull(Box<Expr>, bool), // bool - 'is null' if true. 'is not null' if false.
  Sign(Box<Expr>, bool), // true - Plus, false - Minus
  Cast(Box<Expr>, Box<Ty>, Box<Ty>),

  // Binary Arithmetic Expressions
  And(Box<Expr>,Box<Expr>),
  Or(Box<Expr>,Box<Expr>),
  Comp(CompOp, Box<Expr>, Box<Expr>),
  Arithm(ArithmOp, Box<Expr>, Box<Expr>),
  Concat(Box<Expr>,Box<Expr>),

  // Functions
  Fn(Box<FnDecl>, Vec<Box<Expr>>),
  AggFn(Box<AggFnDecl>, Vec<Box<Expr>>),
  WinFn(Box<WinFnDecl>, Vec<Box<Expr>>),

  // String operators or pattern matching predicates: lhs - pattern, rhs - value  
  Like(Box<Expr>,Box<Expr>, bool), 
  SimilarTo(Box<Expr>,Box<Expr>, bool),
  RegexMatch(Box<Expr>,Box<Expr>, bool),  

  // Other predicates
  Between(Box<Expr>,Box<Expr>,Box<Expr>), // predicand, begin, end
  In(Box<Expr>, Vec<Box<Expr>>), // predicand, a set

  // condition
  Case(Vec<Box<Expr>>, Box<Expr>), // multiple conditions, else return value
  IfThen(Box<Expr>, Box<Expr>), // condition, return value  

  // values  
  Row(Vec<Box<Expr>>),  
  Field(Column),
  Const(Datum)
}

impl Expr {
  pub fn from_column(column: &Column) -> Expr {
    Expr {
      ty: column.ty.clone(),
      node: ExprSpec::Field(column.clone())
    }
  }

  pub fn column<T: AsRef<str>>(name: T, ty: Ty) -> Expr {
    Expr {
      ty: ty.clone(),
      node: ExprSpec::Field(Column::new(name, ty.clone()))
    }
  }  
}

impl HasTy for Expr {
  fn data_ty(&self) -> &Ty {
    &self.ty
  }
}

impl Add for Expr {
  type Output = Expr;

  fn add(self, rhs: Expr) -> Expr {
    Expr {
      ty: result_data_ty(self.data_ty(), rhs.data_ty()),
      node: ExprSpec::Arithm(ArithmOp::Plus, Box::new(self), Box::new(rhs))
    }
  }
}

impl Sub for Expr {
  type Output = Expr;

  fn sub(self, rhs: Expr) -> Expr {
    Expr {
      ty: result_data_ty(self.data_ty(), rhs.data_ty()),
      node: ExprSpec::Arithm(ArithmOp::Sub, Box::new(self), Box::new(rhs))
    }
  }
}

impl Mul for Expr {
  type Output = Expr;

  fn mul(self, rhs: Expr) -> Expr {
    Expr {
      ty: result_data_ty(self.data_ty(), rhs.data_ty()),
      node: ExprSpec::Arithm(ArithmOp::Mul, Box::new(self), Box::new(rhs))
    }
  }
}

impl Div for Expr {
  type Output = Expr;

  fn div(self, rhs: Expr) -> Expr {
    Expr {
      ty: result_data_ty(self.data_ty(), rhs.data_ty()),
      node: ExprSpec::Arithm(ArithmOp::Div, Box::new(self), Box::new(rhs))
    }
  }
}

impl Rem for Expr {
  type Output = Expr;

  fn rem(self, rhs: Expr) -> Expr {
    Expr {
      ty: result_data_ty(self.data_ty(), rhs.data_ty()),
      node: ExprSpec::Arithm(ArithmOp::Rem, Box::new(self), Box::new(rhs))
    }
  }
}

pub trait AsExpr {
  fn as_expr(&self) -> Expr;
}

impl AsExpr for Column {
  fn as_expr(&self) -> Expr {
    Expr {
      ty: self.ty.clone(),
      node: ExprSpec::Field(self.clone())
    }
  }
}

/// Visitor for Expr Tree
pub trait Visitor<'v>: Sized {
  fn visit_not(&mut self, child: &'v Expr) {
    walk_expr(self, child);
  } 
  fn visit_is_null(&mut self, child: &'v Expr, not: bool) {
    walk_expr(self, child);
  }
  fn visit_sign(&mut self, child: &'v Expr, not: bool) {
    walk_expr(self, child);
  }
  fn visit_cast(&mut self, expr: &'v Expr, from: &'v Ty, 
    to: &'v Ty) {
    walk_expr(self, expr);
  }

  fn visit_and(&mut self, lhs: &'v Expr, rhs: &'v Expr) {
    walk_bin_expr(self, lhs, rhs)
  }
  fn visit_or(&mut self, lhs: &'v Expr, rhs: &'v Expr) {
    walk_bin_expr(self, lhs, rhs)
  }
  fn visit_comp(&mut self, op: &CompOp, lhs: &'v Expr, rhs: &'v Expr) {
    walk_bin_expr(self, lhs, rhs)
  }
  fn visit_arithm(&mut self, op: &ArithmOp, 
    lhs: &'v Expr, rhs: &'v Expr) {
    walk_bin_expr(self, lhs, rhs)
  }
  fn visit_concat(&mut self, lhs: &'v Expr, rhs: &'v Expr) {
    walk_bin_expr(self, lhs, rhs)
  }

  fn visit_fn(&mut self, fd: &'v FnDecl, args: &'v Vec<Box<Expr>>) {
    for ref arg in args {
      walk_expr(self, arg);
    }
  }
  fn visit_aggfn(&mut self, aggfd: &'v AggFnDecl, args: &'v Vec<Box<Expr>>) {
    for ref arg in args {
      walk_expr(self, arg);
    }
  }
  fn visit_winfn(&mut self, winfd: &'v WinFnDecl, args: &'v Vec<Box<Expr>>) {
    for ref arg in args {
      walk_expr(self, arg);
    }
  }

  fn visit_like(&mut self, pat: &'v Expr, pred: &'v Expr, not: bool) {
    walk_bin_expr(self, pat, pred);
  }
  fn visit_similarto(&mut self, pat: &'v Expr, pred: &'v Expr, not: bool) {
    walk_bin_expr(self, pat, pred);
  }
  fn visit_regexmatch(&mut self, pat: &'v Expr, pred: &'v Expr, not: bool) {
    walk_bin_expr(self, pat, pred);
  }

  fn visit_between(&mut self, pred: &'v Expr, begin: &'v Expr, end: &'v Expr) {
    walk_expr(self, pred);
    walk_expr(self, begin);
    walk_expr(self, end);
  }
  fn visit_in(&mut self, pred: &'v Expr, values: &'v Vec<Box<Expr>>) {
    walk_expr(self, pred);
    self.visit_row(values);
  }

  fn visit_case(&mut self, ifconds: &'v Vec<Box<Expr>>, else_result: &'v Expr) {
    for ref each_if in ifconds {
      walk_expr(self, each_if);
    }
    walk_expr(self, else_result);
  }
  fn visit_if_then(&mut self, ifcond: &'v Expr, result: &'v Expr) {
    walk_expr(self, ifcond);
    walk_expr(self, result);
  }  

  fn visit_row(&mut self, values: &'v Vec<Box<Expr>>) {
    for ref v in values {
      walk_expr(self, v);
    }
  }
  fn visit_field(&mut self, field: &'v Column) {}
  fn visit_const(&mut self, datum: &'v Datum) {}
}

/// Walker for Expr Tree
pub fn walk_expr<'v, V: Visitor<'v>>(visitor: &mut V, expr: &'v Expr) {

  match expr.node {

    ExprSpec::Not(ref child) => {
      visitor.visit_not(child);
    }

    ExprSpec::IsNull(ref child, positive) => {
      visitor.visit_is_null(child, positive);
    }

    ExprSpec::Sign(ref child, positive) => {
      visitor.visit_sign(child, positive);
    }

    ExprSpec::Cast(ref value, ref from, ref to) => {
      visitor.visit_cast(value, from, to)
    },


    ExprSpec::And(ref lhs, ref rhs) => {
      visitor.visit_and(lhs, rhs);
    }

    ExprSpec::Or(ref lhs, ref rhs) => {
      visitor.visit_or(lhs, rhs);
    }

    ExprSpec::Comp(ref op, ref lhs, ref rhs) => {
      visitor.visit_comp(op, lhs, rhs);
    }

    ExprSpec::Arithm(ref op, ref lhs, ref rhs) => {
      visitor.visit_arithm(op, lhs, rhs);
    }

    ExprSpec::Concat(ref lhs, ref rhs) => {
      visitor.visit_concat(lhs, rhs);
    }


    ExprSpec::Fn(ref fd, ref args) => {
      visitor.visit_fn(fd, args);
    },
    
    ExprSpec::AggFn(ref fd, ref args) => {
      visitor.visit_aggfn(fd, args);
    },

    ExprSpec::WinFn(ref fd, ref args) => {
      visitor.visit_winfn(fd, args);
    },


    ExprSpec::Like(ref pat, ref pred, not) => {
      visitor.visit_like(pat, pred, not)
    },

    ExprSpec::SimilarTo(ref pat, ref pred, not) => {
      visitor.visit_similarto(pat, pred, not)
    },

    ExprSpec::RegexMatch(ref pat, ref pred, not) => {
      visitor.visit_regexmatch(pat, pred, not)
    },


    ExprSpec::Between(ref pred, ref begin, ref end) => {
      visitor.visit_between(pred, begin, end)
    }

    ExprSpec::In(ref pred, ref values) => {
      visitor.visit_in(pred, values)
    }


    ExprSpec::Case(ref ifconds, ref else_result) => {
      visitor.visit_case(ifconds, else_result)
    }

    ExprSpec::IfThen(ref ifcond, ref result) => {
      visitor.visit_if_then(ifcond, result)
    },
    

    ExprSpec::Row(ref values) => visitor.visit_row(values),

    ExprSpec::Field(ref column) => visitor.visit_field(column),

    ExprSpec::Const(ref datum) => visitor.visit_const(datum),
  }
}

/// Walker for Binary Expr
#[inline]
pub fn walk_bin_expr<'v, V: Visitor<'v>>(visitor: &mut V, 
                                         lhs: &'v Expr, 
                                         rhs: &'v Expr) {
  walk_expr(visitor, lhs);
  walk_expr(visitor, rhs);
}