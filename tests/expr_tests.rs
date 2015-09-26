extern crate tajo;
extern crate common;

use self::common::types::*;
use tajo::schema::Column;
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
      ArithmOp::Sub => self.order.push("minus".to_string()),
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

  let col1 = Column::new("c0", *INT8_TY);
  let col2 = Column::new("c1", *INT4_TY);

  let col3 = Column::new("c2", *INT4_TY);
  let col4 = Column::new("c3", *INT4_TY);

  let plus1 = col1.as_expr() + col2.as_expr();
  let plus2 = col3.as_expr() + col4.as_expr();
  let plus3 = plus1 * plus2;

  let mut visitor = VisitOrder{order: Vec::new()};
  walk_expr(&mut visitor, &plus3);

  assert_eq!(vec!["c0", "c1", "plus", "c2", "c3", "plus", "mul"], visitor.order);
}
