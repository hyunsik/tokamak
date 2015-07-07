//!
//! Expression Plan Representation for Tajo Kernel
//!

use common::schema::Column;

enum Expr {
  // Unary Expressions
  Not {child: Box<Expr>},
  isNull {child: Box<Expr>},

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

  Signed,
  Cast,
  Row,
  Field {field: Column},
  Const
}