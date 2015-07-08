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

pub enum BinCompOp {
  Eq,
  NotEq,
  Lth,
  Leq,
  Gth,
  Geq
}

pub enum BinArithOp {
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
  IsNull(Box<Expr>,Box<Expr>, bool), // bool - 'is not null' if true
  Signed(Box<Expr>),

  // Binary Arithmetic Expressions
  And(Box<Expr>,Box<Expr>),
  Or(Box<Expr>,Box<Expr>),
  Comp(BinCompOp, Box<Expr>,Box<Expr>),
  BinArithmetic(BinArithOp,Box<Expr>,Box<Expr>),
  Concatenate(Box<Expr>,Box<Expr>),

  // Functions
  Function(Box<FnDecl>, Vec<Box<Expr>>),
  AggFunction(Box<AggFnDecl>, Vec<Box<Expr>>),
  WindowFunction(Box<WinFnDecl>, Vec<Box<Expr>>),

  // String operators or pattern matching predicates: lhs - pattern, rhs - value  
  Like(Box<Expr>,Box<Expr>), 
  SimilarTo(Box<Expr>,Box<Expr>),
  RegexMatch(Box<Expr>,Box<Expr>),  

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

  fn visit_and(&mut self, lhs: &'v Expr, rhs: &'v Expr);
  fn visit_or(&mut self, lhs: &'v Expr, rhs: &'v Expr);
  fn visit_comp(&mut self, lhs: &'v Expr, rhs: &'v Expr);
  fn visit_bin_arith_op(&mut self, op: &BinArithOp, lhs: &'v Expr, rhs: &'v Expr);
  fn visit_concat(&mut self, lhs: &'v Expr, rhs: &'v Expr);

  fn visit_fn(&mut self, fd: &'v FnDecl, args: &'v Vec<Expr>);
  fn visit_aggfn(&mut self, aggfd: &'v AggFnDecl, args: &'v Vec<Expr>);
  fn visit_winfn(&mut self, winfd: &'v WinFnDecl, args: &'v Vec<Expr>);

  fn visit_like(&mut self, pattern: &'v Expr, pred: &'v Expr, not: bool);
  fn visit_similarto(&mut self, pattern: &'v Expr, pred: &'v Expr, not: bool);
  fn visit_regexmatch(&mut self, pattern: &'v Expr, pred: &'v Expr, not: bool);

  fn visit_between(&mut self, pred: &'v Expr, begin: &'v Expr, end: &'v Expr);
  fn visit_in(&mut self, pred: &'v Expr, &'v Vec<Expr>);

  fn visit_case(&mut self, ifconds: &'v Vec<Box<Expr>>, else_result: &'v Expr);
  fn visit_if_then(&mut self, ifcond: &'v Expr, result: &'v Expr);

  fn visit_cast(&mut self, expr: &'v Expr, from: &'v DataType, to: &'v DataType);
  fn visit_row(&mut self, row: &'v Vec<Box<Expr>>);

  fn visit_field(&mut self, field: &'v Column);
  fn visit_const(&mut self, datum: &'v Datum);
}