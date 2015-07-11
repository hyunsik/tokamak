//!
//! Interpreter Compiler for Expressions
//!

use common::err::{Error, TResult, Void, void_ok};
use eval::Eval;
use expr::{Datum, Expr, Visitor};
use schema::{Column, Schema};
use std::boxed::Box;
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
pub struct Plus {data_ty: DataTy, pub lhs: Box<Eval>, pub rhs: Box<Eval>}
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
pub struct Const {datum: Datum, res_type: DataTy}


impl Eval for Plus {
  fn bind(&mut self, schema: &Schema) -> Void {
    self.data_ty = result_data_ty(self.lhs.data_ty(), self.rhs.data_ty());
    void_ok()
  }  
  
  fn is_const(&self) -> bool { false }
}

impl HasDataTy for Plus {
  #[inline]
  fn data_ty(&self) -> &DataTy {
    &self.data_ty
  }
}

impl Field {
  pub fn new(column: &Column) -> Field {
    Field {
      column: column.clone(),
      field_id: None
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
      self.field_id = Some(id);
      void_ok()
     },
     
     None => Err(Error::UndefinedColumn)
    }    
  }  
  
  fn is_const(&self) -> bool { false }

  // fn eval(&self, RowBlock) -> TResult<&Vector1>;  
}

impl Const {
  fn new(datum: &Datum) -> Const {
    Const {
        datum: datum.clone(), 
        res_type: DataTy::new(datum.ty())
    }
  }
}

// impl Eval for Const {
  
//   fn bind(&mut self, schema: &Schema) -> Void {
//     self.res_type = DataTy::new(self.datum.ty());
//     void_ok()
//   }

//   fn ty(&self) -> &DataTy {
//     &self.res_type
//   }
  
//   fn is_const(&self) -> bool { true }
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