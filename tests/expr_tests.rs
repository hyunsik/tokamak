extern crate tajo;

use tajo::schema::Column;
use tajo::types::*;
use tajo::expr::*;

struct VisitOrder {
  pub order: Vec<String>
}

impl<'v> Visitor<'v> for VisitOrder {
  fn visit_arithm(&mut self, op: &ArithmOp, lhs: &Expr, rhs: &Expr) {
    walk_expr(self, lhs);
    walk_expr(self, rhs);

    match *op {
      ArithmOp::Plus => self.order.push("plus".to_string()),
      ArithmOp::Minus => self.order.push("minus".to_string()),
      ArithmOp::Mul => self.order.push("mul".to_string()),
      ArithmOp::Div => self.order.push("div".to_string()),
      _ => panic!("Unknown op")
    }
  }

  fn visit_field(&mut self, c: &'v Column) {
    self.order.push(format!("{}", c.name)); 
  }
}

#[test]
fn test_visit_arithm() {

  let col1 = Column::new("c0", Ty::Int8);
  let col2 = Column::new("c1", Ty::Int4);

  let col3 = Column::new("c2", Ty::Int4);
  let col4 = Column::new("c3", Ty::Int4);
  
  let lhs1 : Box<Expr> = Box::new(Expr::new_field(&col1));
  let rhs2 : Box<Expr> = Box::new(Expr::new_field(&col2));
  
  let plus1: Box<Expr> = Box::new(Expr::new_arithm(ArithmOp::Plus, lhs1, rhs2));

  let lhs2 : Box<Expr> = Box::new(Expr::new_field(&col3));
  let rhs2 : Box<Expr> = Box::new(Expr::new_field(&col4));

  let plus2: Box<Expr> = Box::new(Expr::new_arithm(ArithmOp::Plus, lhs2, rhs2));

  let plus3: Box<Expr> = Box::new(Expr::new_arithm(ArithmOp::Mul, plus1, plus2));
 
  let mut visitor = VisitOrder{order: Vec::new()};  
  walk_expr(&mut visitor, &*plus3);

  assert_eq!(vec!["c0", "c1", "plus", "c2", "c3", "plus", "mul"], visitor.order);
}