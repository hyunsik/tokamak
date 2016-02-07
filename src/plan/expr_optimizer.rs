use super::expr::*;
use super::expr::visitor::*;

pub fn transform_or<V, F>(v: &V, cond: bool, e: &Expr, f: F) -> Expr
  where V: visitor::TransformVisitor + Sized,
        F: Fn(&Expr) -> Expr
{
  if cond {
    f(e)
  } else {
    visitor::transform_by_default(v, e)
  }
}

pub fn transform_bool_or<F>(l: &Literal, f: F) -> Expr
  where F: Fn(bool) -> Expr
{
  match *l {
    Literal::Bool(value) => f(value),
    _ => Const(l.clone()),
  }
}


pub struct CastSimplification;

impl TransformVisitor for CastSimplification {
  fn transform(&self, e: &Expr) -> Expr {
    match *e.kind() {
      ExprKind::Cast(ref c, ref f, ref t) => transform_or(self, f == t, c, |c| c.clone()),
      _ => transform_by_default(self, e),
    }
  }
}

pub struct BooleanSimplification;

impl TransformVisitor for BooleanSimplification {
  fn transform(&self, e: &Expr) -> Expr {
    match *e.kind() {

      ExprKind::Not(ref c) => {
        match *c.kind() {

          ExprKind::Not(ref grand_child) => clone(grand_child),

          ExprKind::And(ref l, ref r) => Or(Not(clone(l)), Not(clone(r))),
          ExprKind::Or(ref l, ref r) => And(Not(clone(l)), Not(clone(r))),

          ExprKind::Cmp(ref o, ref l, ref r) => {
            match *o {
              CmpOp::Eq => Cmp(CmpOp::Ne, clone(l), clone(r)),
              CmpOp::Ne => Cmp(CmpOp::Eq, clone(l), clone(r)),
              CmpOp::Lt => Cmp(CmpOp::Ge, clone(l), clone(r)),
              CmpOp::Le => Cmp(CmpOp::Gt, clone(l), clone(r)),
              CmpOp::Gt => Cmp(CmpOp::Le, clone(l), clone(r)),
              CmpOp::Ge => Cmp(CmpOp::Lt, clone(l), clone(r)),
            }
          }

          ExprKind::Const(ref l) => transform_bool_or(l, |b| Const(Literal::Bool(!b))),
          _ => transform_by_default(self, e),
        }
      }
      _ => transform_by_default(self, e),
    }
  }
}
