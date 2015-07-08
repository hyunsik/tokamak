//!
//! Expression Plan Representation for Tajo Kernel
//!

use common::types::{DataType, HasTypeKind, TypeKind};
use common::schema::Column;
use common::P;

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

impl HasTypeKind for Datum {
  fn type_kind(&self) -> TypeKind {
    match *self {
      Datum::Bool(ref x) => TypeKind::Bool,
      Datum::Int1(ref x) => TypeKind::Int1,
      Datum::Int2(ref x) => TypeKind::Int2,
      Datum::Int4(ref x) => TypeKind::Int4,
      Datum::Int8(ref x) => TypeKind::Int8,
      Datum::Float4(ref x) => TypeKind::Float4,
      Datum::Float8(ref x) => TypeKind::Float8,
      Datum::Time(ref x) => TypeKind::Time,
      Datum::Date(ref x) => TypeKind::Date,
      Datum::Timestamp(ref x) => TypeKind::Timestamp,
      Datum::Interval(ref x,ref y) => TypeKind::Interval,
      Datum::Char(ref x) => TypeKind::Char,
      Datum::Text(ref x) => TypeKind::Text,
      Datum::Varchar(ref x) => TypeKind::Varchar,
      Datum::Blob(ref x) => TypeKind::Blob
    }
  }
}

pub struct FnDecl {
  signature: String
}

pub struct AggFnDecl {
  signature: String 
}

pub struct WinFnDecl {
  signature: String  
}

pub enum CompOp {
  Eq,
  NotEq,
  Lth,
  Leq,
  Gth,
  Geq
}

pub enum ArithmOp {
  Plus,
  Minus,
  Multiply,
  Divide,
  Modular,  
}

pub struct Expr {
  data_ty: DataType,
  node: ExprSpec
}

pub enum ExprSpec {
  // Unary Expressions
  Not(Box<Expr>),
  IsNull(Box<Expr>, bool), // bool - 'is null' if true. 'is not null' if false.
  Sign(Box<Expr>, bool), // true - Plus, false - Minus

  // Binary Arithmetic Expressions
  And(Box<Expr>,Box<Expr>),
  Or(Box<Expr>,Box<Expr>),
  Comp(CompOp, Box<Expr>,Box<Expr>),
  Arithm(ArithmOp,Box<Expr>,Box<Expr>),
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
  Cast(Box<Expr>,Box<DataType>,Box<DataType>),
  Row(Vec<Box<Expr>>),  
  Field(Box<Column>),
  Const(Box<Datum>)
}

pub trait Visitor<'v>: Sized {
  fn visit_not(&mut self, child: &'v Expr);  
  fn visit_is_null(&mut self, child: &'v Expr, not: bool);
  fn visit_sign(&mut self, child: &'v Expr, not: bool);

  fn visit_and(&mut self, lhs: &'v Expr, rhs: &'v Expr);
  fn visit_or(&mut self, lhs: &'v Expr, rhs: &'v Expr);
  fn visit_comp(&mut self, op: &CompOp, lhs: &'v Expr, rhs: &'v Expr);
  fn visit_arithm(&mut self, op: &ArithmOp, 
    lhs: &'v Expr, rhs: &'v Expr);
  fn visit_concat(&mut self, lhs: &'v Expr, rhs: &'v Expr);

  fn visit_fn(&mut self, fd: &'v FnDecl, args: &'v Vec<Box<Expr>>);
  fn visit_aggfn(&mut self, aggfd: &'v AggFnDecl, args: &'v Vec<Box<Expr>>);
  fn visit_winfn(&mut self, winfd: &'v WinFnDecl, args: &'v Vec<Box<Expr>>);

  fn visit_like(&mut self, pat: &'v Expr, pred: &'v Expr, not: bool);
  fn visit_similarto(&mut self, pat: &'v Expr, pred: &'v Expr, not: bool);
  fn visit_regexmatch(&mut self, pat: &'v Expr, pred: &'v Expr, not: bool);

  fn visit_between(&mut self, pred: &'v Expr, begin: &'v Expr, end: &'v Expr);
  fn visit_in(&mut self, pred: &'v Expr, &'v Vec<Box<Expr>>);

  fn visit_case(&mut self, ifconds: &'v Vec<Box<Expr>>, else_result: &'v Expr);
  fn visit_if_then(&mut self, ifcond: &'v Expr, result: &'v Expr);

  fn visit_cast(&mut self, expr: &'v Expr, from: &'v DataType, 
    to: &'v DataType);
  fn visit_row(&mut self, row: &'v Vec<Box<Expr>>);
  fn visit_field(&mut self, field: &'v Column);
  fn visit_const(&mut self, datum: &'v Datum);
}

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


    ExprSpec::Cast(ref value, ref from, ref to) => {
      visitor.visit_cast(value, from, to)
    },

    ExprSpec::Row(ref values) => visitor.visit_row(values),

    ExprSpec::Field(ref column) => visitor.visit_field(column),

    ExprSpec::Const(ref datum) => visitor.visit_const(datum),
  }
}