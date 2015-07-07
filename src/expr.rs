//!
//! Expression Plan Representation for Tajo Kernel
//!

use common::types::{HasTypeKind, TypeKind};
use common::schema::Column;

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

pub enum Expr {
  // Unary Expressions
  Not {child: Box<Expr>},
  IsNull {child: Box<Expr>},

  // Binary Arithmetic Expressions
  And {lhs: Box<Expr>, rhs: Box<Expr>},
  Or {lhs: Box<Expr>, rhs: Box<Expr>},
  Equal {lhs: Box<Expr>, rhs: Box<Expr>},
  NotEqual {lhs: Box<Expr>, rhs: Box<Expr>},
  LessThan {lhs: Box<Expr>, rhs: Box<Expr>},
  LessThanOrEqual {lhs: Box<Expr>, rhs: Box<Expr>},
  GreaterThan {lhs: Box<Expr>, rhs: Box<Expr>},
  GreaterThanOrEqual {lhs: Box<Expr>, rhs: Box<Expr>},

  Plus {lhs: Box<Expr>, rhs: Box<Expr>},
  Minus {lhs: Box<Expr>, rhs: Box<Expr>},
  Multiply {lhs: Box<Expr>, rhs: Box<Expr>},
  Divide {lhs: Box<Expr>, rhs: Box<Expr>},
  Modular {lhs: Box<Expr>, rhs: Box<Expr>},

  // Functions
  Function,
  AggFunction,
  WindowFunction,

  // String operators or pattern matching predicates
  Like {pattern: String, child: Box<Expr>},
  SimilarTo {pattern: String, child: Box<Expr>},
  RegexMatch {pattern: String, child: Box<Expr>},
  Concatenate {lhs: Box<Expr>, rhs: Box<Expr>},

  // Other predicates
  Between {lhs: Box<Expr>, mid: Box<Expr>, rhs: Box<Expr>},
  Case,
  IfThen,
  In,

  // Values
  Signed,
  Cast,
  Row,
  Field {column: Column},
  Const {datum: Datum}
}
