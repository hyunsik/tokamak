//!
//! Expression Evaluator Trait and Implementation
//!

use common::err::{Error, TResult, Void, void_ok};
use rows::RowBlock;
use rows::vector::Vector1;
use common::types::{DataType, TypeKind, HasTypeKind};
use common::schema::{Column, Schema};
use expr::{Datum, Expr};

/// Common Trait of All Expression Evaluators  
pub trait Eval {
  fn bind(&mut self, schema: &Schema) -> Void;
  
  fn ty(&self) -> &DataType;

  fn is_const(&self) -> bool;
}

/// Map Expression Evaluator Trait
pub trait MapEval {
  fn eval(&self, RowBlock) -> TResult<&Vector1>;
}

/// Filter Expression Evaluator Trait
pub trait FilterEval {
  fn filter(&self, RowBlock) -> Void;
}

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
pub struct Plus {lhs: Box<Eval>, rhs: Box<Eval>}
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
pub struct Field {column: Column, field_id: Option<usize>}
pub struct Const {datum: Datum, res_type: DataType}

impl Eval for Plus {
  fn bind(&mut self, schema: &Schema) -> Void {
    //self.field_id = schema.get_

    void_ok()
  }

  fn ty(&self) -> &DataType {
    self.lhs.ty()
  }
  
  fn is_const(&self) -> bool { false }
}

impl Field {
  fn new(column: Column) -> Field {
    Field {
      column: column,
      field_id: None
    }
  }
}

impl Eval for Field {
  fn bind(&mut self, schema: &Schema) -> Void {
    //self.field_id = schema.get_

    void_ok()
  }

  fn ty(&self) -> &DataType {
    &self.column.data_type
  }
  
  fn is_const(&self) -> bool { false }

  // fn eval(&self, RowBlock) -> TResult<&Vector1>;  
}

impl Const {
  fn new(datum: &Datum) -> Const {
    Const {
        datum: datum.clone(), 
        res_type: DataType::new(datum.type_kind())
    }
  }
}

impl Eval for Const {
  
  fn bind(&mut self, schema: &Schema) -> Void {
    self.res_type = DataType::new(self.datum.type_kind());    
    void_ok()
  }

  fn ty(&self) -> &DataType {
    &self.res_type
  }
  
  fn is_const(&self) -> bool { true }
}

// pub fn walk_expr<'a>(expr: &'a Expr) {
//   match *expr {
//     Expr::Plus (ref lhs, ref rhs) => {
//       walk_expr(&**lhs);
//       walk_expr(&**rhs);
//     },
//     _ => {}
//   };
// }

pub fn compile<'a>(expr: &'a Expr) -> TResult<Box<Eval>> {  
  match *expr {

    // Expr::Between {lhs, mid, rhs} => {
    //   Ok(Box::new(
    //     Between {
    //       lhs: try!(compile(lhs)), 
    //       mid: try!(compile(mid)), 
    //       rhs: try!(compile(rhs))
    //     })
    //   )
    // },

    // Expr::Plus (ref lhs, ref rhs) => {
    //   Ok(Box::new(
    //     Plus {
    //       lhs: try!(compile(&**lhs)),
    //       rhs: try!(compile(&**rhs)),
    //     }
    //   ))
    // },

    // Expr::Field {column} => Ok(Box::new(Field::new(column))),

    // Expr::Const {datum} => Ok(Box::new(Const::new(&datum))),

    _ => Err(Error::InvalidExpression)
  }
}

pub fn map_plus_vec_vec<RES, L, R>(lhs: Vec<L>, rhs: Vec<R>) {
}