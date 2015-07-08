extern crate tajo;

use tajo::schema::Column;
use tajo::types::*;
use tajo::expr::*;
use tajo::eval::*;
use std::marker;

struct VisitOrder {
  order: i32,
}

impl<'v> Visitor<'v> for VisitOrder {
  fn visit_arithm(&mut self, op: &ArithmOp, lhs: &Expr, rhs: &Expr) {
    walk_expr(self, lhs);
    walk_expr(self, rhs);

    match *op {
      ArithmOp::Plus => println!("Plus"),
      ArithmOp::Minus => println!("Minus"),
      _ => panic!("Unknown op")
    }
  }

  fn visit_field(&mut self, c: &'v Column) {
    println!("column: {}", c.name); 
  }
}

#[test]
fn test_visitor() {

  let col1 = Column::new("c0".to_string(), Ty::Int8);
  let col2 = Column::new("c1".to_string(), Ty::Int4);

  let col3 = Column::new("c2".to_string(), Ty::Int4);
  let col4 = Column::new("c3".to_string(), Ty::Int4);
  
  let lhs1 : Box<Expr> = Box::new(Expr::new_field(&col1));
  let rhs2 : Box<Expr> = Box::new(Expr::new_field(&col2));
  
  let plus1: Box<Expr> = Box::new(Expr::new_arithm(ArithmOp::Plus, lhs1, rhs2));

  let lhs2 : Box<Expr> = Box::new(Expr::new_field(&col3));
  let rhs2 : Box<Expr> = Box::new(Expr::new_field(&col4));

  let plus2: Box<Expr> = Box::new(Expr::new_arithm(ArithmOp::Plus, lhs2, rhs2));

  let plus3: Box<Expr> = Box::new(Expr::new_arithm(ArithmOp::Plus, plus1, plus2));

  //let lhs: Box<Eval> = Box::new(Field::new(&col1));
  //let rhs: Box<Eval> = Box::new(Field::new(&col2));
  //let plus: Box<Eval> = Box::new(Plus {lhs: lhs, rhs: rhs});
  
  let mut visitor = VisitOrder{order: 1};  
  walk_expr(&mut visitor, &*plus3);
}