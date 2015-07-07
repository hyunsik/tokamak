use common::err::{Error, TResult, Void, void_ok};
use rows::RowBlock;
use rows::vector::Vector1;
use common::types::{DataType, TypeKind, HasTypeKind};
use common::schema::{Column, Schema};
use expr::{Datum, Expr};

pub trait Eval {
  fn bind(&mut self, schema: &Schema) -> Void;

  fn datatype(&self) -> &DataType;

  // fn eval(&self, RowBlock) -> TResult<&Vector1>;  
  fn is_const(&self) -> bool;
}

// Unary Expressions
struct Not {child: Box<Eval>}
struct IsNull {child: Box<Eval>}

// Binary Comparison Expressions
struct And {lhs: Box<Eval>, rhs: Box<Eval>}
struct Or {lhs: Box<Eval>, rhs: Box<Eval>}
struct Equal {lhs: Box<Eval>, rhs: Box<Eval>}
struct NotEqual {lhs: Box<Eval>, rhs: Box<Eval>}
struct LessThan {lhs: Box<Eval>, rhs: Box<Eval>}
struct LessThanOrEqual {lhs: Box<Eval>, rhs: Box<Eval>}
struct GreaterThan {lhs: Box<Eval>, rhs: Box<Eval>}
struct GreaterThanOrEqual {lhs: Box<Eval>, rhs: Box<Eval>}

// Binary Arithmetic Evalessions
struct Plus {lhs: Box<Eval>, rhs: Box<Eval>}
struct Minus {lhs: Box<Eval>, rhs: Box<Eval>}
struct Multiply {lhs: Box<Eval>, rhs: Box<Eval>}
struct Divide {lhs: Box<Eval>, rhs: Box<Eval>}
struct Modular {lhs: Box<Eval>, rhs: Box<Eval>}

// String operators or pattern matching predicates
struct Like {pattern: String, child: Box<Eval>}
struct SimilarTo {pattern: String, child: Box<Eval>}
struct RegexMatch {pattern: String, child: Box<Eval>}
struct Concatenate {lhs: Box<Eval>, rhs: Box<Eval>}

struct Between {lhs: Box<Eval>, mid: Box<Eval>, rhs: Box<Eval>}

struct Field {column: Column}
struct Const {datum: Datum, res_type: DataType}

impl Eval for Field {
  fn bind(&mut self, schema: &Schema) -> Void {
    void_ok()
  }

  fn datatype(&self) -> &DataType {
    &self.column.data_type
  }

  // fn eval(&self, RowBlock) -> TResult<&Vector1>;  
  fn is_const(&self) -> bool { false }
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

  fn datatype(&self) -> &DataType {
    &self.res_type
  }
  
  fn is_const(&self) -> bool { true }
}

pub fn compile(expr: Box<Expr>) -> TResult<Box<Eval>> {  
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

    Expr::Field {column} => Ok(Box::new(Field {column: column})),

    Expr::Const {datum} => Ok(Box::new(Const::new(datum))),

    _ => Err(Error::InvalidExpression)
  }
}

