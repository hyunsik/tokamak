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
	  	ExprKind::Not(ref c) => transform_bool_or(c, |b| Const(Literal::Bool(!b))),
	  	ExprKind::Comp(ref o, ref l, ref r) => match *o {
	  		_ => panic!("")
	  	},
	  	_ =>  transform_by_default(self, e)
	  }
	}
}

//match literal {
//	  			Literal::Bool(v) => match v {
//	  				true => Literal::Bool(false)),
//	  				false => Literal::Bool(true))
//	  			},
//	  			_ => literal.clone()
//	  			
//	  		}