use common::err::Result;

use super::expr::*;
use super::expr::visitor::*;

pub struct SimplifyCast;

impl TransformVisitor for SimplifyCast
{
	fn transform(&self, e: &Expr) -> Expr 
  {
	  match *e.spec() {
	  	ExprSpec::Cast(ref c, ref f, ref t) => {
	  		transform_or(self, f == t, e, |e| e.clone())
	  	},
	  	_ =>  transform_by_default(self, e)
	  }
	}
}

pub struct BooleanSimplification;