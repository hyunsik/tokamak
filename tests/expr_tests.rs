extern crate tajo;

use tajo::schema::Column;
use tajo::types::*;
use tajo::expr::*;
use tajo::eval::*;

struct VisitOrder {
  order: Vec<Box<String>>
}

impl<'v> Visitor<'v> for VisitOrder {
  fn visit_field(&mut self, c: &'v Column) {
    println!("column: {}", c.name); 
  } 
}

#[test]
fn test_visitor() {

  let col1 = Column::new("c0".to_string(), Ty::Int8);
  let col2 = Column::new("c1".to_string(), Ty::Int4);
  
  let lhs : Box<Expr> = Box::new(Expr::new_field(&col1));
  let rhs : Box<Expr> = Box::new(Expr::new_field(&col2));
  
  //let lhs: Box<Eval> = Box::new(Field::new(&col1));
  //let rhs: Box<Eval> = Box::new(Field::new(&col2));
  //let plus: Box<Eval> = Box::new(Plus {lhs: lhs, rhs: rhs});
  
  //let mut visitor = VisitOrder{order: Vec::new()};  
  //walk_expr(&visitor, plus);
}