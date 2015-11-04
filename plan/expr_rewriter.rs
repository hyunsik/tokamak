use super::expr::*;
use super::expr::visitor::*;

pub struct SimplifyNot;

pub trait Rule
{
	fn rewrite(&mut self, e: &Expr) -> Expr;
}

pub struct Rewriter {
	rule: Box<Rule>,
	stack: Vec<Box<Expr>>
}

impl Rewriter
{
	pub fn rewrite(&mut self, e: &Expr) -> Box<Expr>
	{
		rewrite_by_default(self, e)
	}
	
	pub fn push(&mut self, e: Expr)
	{
		self.stack.push(Box::new(e));
	}
}

impl SimpleVisitor for Rewriter
{
	fn accept(&mut self, e: &Expr) 
  { 
  	let rewritten = match *e.spec() {
  		ExprSpec::Not (ref child) => Expr(e.0.clone(), ExprSpec::Not(self.rewrite(child))),
  		_ => panic!("")
  	};
  	
  	self.push(rewritten);
  }
}



impl SimpleVisitor for SimplifyNot 
{
  fn accept(&mut self, e: &Expr) 
  { 
	  let expr = match *e.spec() {
		 	ExprSpec::Not (ref child) => match *child.spec() {
		 		ExprSpec::Not (ref child) => child.clone(),
		 		_ => rewrite_by_default(self, e)
	 		},
	 		_ => rewrite_by_default(self, e)
	  };
	}
}

pub fn rewrite_by_default<T>(v: &mut T, e: &Expr) -> Box<Expr> 
		where T: SimpleVisitor + Sized
{
	Box::new(e.clone())
} 