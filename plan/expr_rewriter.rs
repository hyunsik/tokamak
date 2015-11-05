use common::err::Result;

use super::expr::*;
use super::expr::visitor::*;

pub struct CastSimplification;

impl TransformVisitor for CastSimplification
{
	fn transform(&self, e: &Expr) -> Expr 
  {
	  match *e.kind() {
	  	ExprKind::Cast(ref c, ref f, ref t) => {
	  		transform_or(self, f == t, e, |e| e.clone())
	  	},
	  	_ =>  transform_by_default(self, e)
	  }
	}
}

pub struct BooleanSimplification;

impl TransformVisitor for BooleanSimplification
{
	fn transform(&self, e: &Expr) -> Expr 
  {
	  match *e.kind() {
	  	
	  	ExprKind::Not(ref c) => match *c.kind() {
	  	  ExprKind::Not(ref grand_child) => clone(grand_child),
	  	  _ => transform_bool_or(c, |b| Const(Literal::Bool(!b)))
  		},
	  	
	  	ExprKind::And(ref l, ref r) => Or (Not(clone(l)), Not(clone(r))),
	  	ExprKind::Or (ref l, ref r) => And(Not(clone(l)), Not(clone(r))),
	  	
	  	ExprKind::Cmp(ref o, ref l, ref r) => match *o {
	  		CmpOp::Eq => Cmp(CmpOp::Ne, clone(l), clone(r)),
	  		CmpOp::Ne => Cmp(CmpOp::Eq, clone(l), clone(r)),
	  		CmpOp::Lt => Cmp(CmpOp::Ge, clone(l), clone(r)),
	  		CmpOp::Le => Cmp(CmpOp::Gt, clone(l), clone(r)),
	  		CmpOp::Gt => Cmp(CmpOp::Le, clone(l), clone(r)),
	  		CmpOp::Ge => Cmp(CmpOp::Lt, clone(l), clone(r)),
	  	},
	  	_ =>  e.clone()
	  }
	}
}