use common::types::Ty;

#[derive(Clone)]
pub struct Expr (pub Ty, pub ExprSpec);

impl Expr 
{
	pub fn spec(&self) -> &ExprSpec
	{
		&self.1
	}
}

/// Expression Specific Element
#[derive(Clone)]
pub enum ExprSpec {
  // Unary Expressions
  Not(Box<Expr>),
  IsNull(Box<Expr>, bool), // bool - 'is null' if true. 'is not null' if false.
  Sign(Box<Expr>, bool), // true - Plus, false - Minus
  Cast(Box<Expr>, Box<Ty>, Box<Ty>),

  // Binary Arithmetic Expressions
  And(Box<Expr>,Box<Expr>),
  Or(Box<Expr>,Box<Expr>),
  Comp(CompOp, Box<Expr>, Box<Expr>),
  Arithm(ArithmOp, Box<Expr>, Box<Expr>),

  // Functions
  Fn(Box<FnDecl>, Vec<Box<Expr>>),

  // Other predicates
  Between(Box<Expr>,Box<Expr>,Box<Expr>), // predicand, begin, end

  // condition
  Switch(Vec<Box<Expr>>, Box<Expr>), // multiple conditions, else return value
  Case  (Box<Expr>, Box<Expr>),      // condition, return value

  // values
  Field(String),
  Const(Literal)
}

/// Comparison Operator Type
#[derive(Clone, Copy)]
pub enum CompOp {
  Eq,
  Ne,
  Lt,
  Le,
  Gt,
  Ge
}

/// Arithmetic Operator Type
#[derive(Clone, Copy)]
pub enum ArithmOp {
  Plus,
  Sub,
  Mul,
  Div,
  Rem,
}

/// Function Declaration
#[derive(Clone)]
pub struct FnDecl {
  signature: String
}

/// Representation for a single value
#[derive(Clone)]
pub enum Literal 
{
  Bool(bool),
  Int1(i8),
  Int2(i16),
  Int4(i32),
  Int8(i64),
  Float4(f32),
  Float8(f64),
  Time(i64),
  Date(i32),
  Timestamp(i64),
  Interval(i64, i32),
  Char(String),
  Text(String),
  Varchar(String),
  Blob(Vec<u8>)
}

pub mod visitor {
	//! Visitor for Expr
	
	use common::types::Ty;
	use super::*;
	
	/// Visitor Trait for Expr Tree. You should implement this trait for walking Expr trees.
	pub trait Visitor<'v, T>: Sized {
	  fn visit_not(&self, ctx: &mut T, child: &'v Expr) {
	    walk_expr(self, ctx, child);
	  }
	  fn visit_is_null(&self, ctx: &mut T, child: &'v Expr, not: bool) {
	    walk_expr(self, ctx, child);
	  }
	  fn visit_sign(&self, ctx: &mut T, child: &'v Expr, not: bool) {
	    walk_expr(self, ctx, child);
	  }
	  fn visit_cast(&self, ctx: &mut T, expr: &'v Expr, from: &'v Ty, to: &'v Ty) {
	    walk_expr(self, ctx, expr);
	  }
	
	  fn visit_and(&self, ctx: &mut T, lhs: &'v Expr, rhs: &'v Expr) {
	    walk_expr(self, ctx, lhs);
	    walk_expr(self, ctx, rhs);
	  }
	  fn visit_or(&self, ctx: &mut T, lhs: &'v Expr, rhs: &'v Expr) {
	    walk_expr(self, ctx, lhs);
	    walk_expr(self, ctx, rhs);
	  }
	  fn visit_comp(&self, ctx: &mut T, op: &CompOp, lhs: &'v Expr, rhs: &'v Expr) {
	    walk_expr(self, ctx, lhs);
	    walk_expr(self, ctx, rhs);
	  }
	  fn visit_arithm(&self, ctx: &mut T, op: &ArithmOp, lhs: &'v Expr, rhs: &'v Expr) {
	    walk_expr(self, ctx, lhs);
	    walk_expr(self, ctx, rhs);
	  }
	  fn visit_concat(&self, ctx: &mut T, lhs: &'v Expr, rhs: &'v Expr) {
	    walk_expr(self, ctx, lhs);
	    walk_expr(self, ctx, rhs);
	  }
	
	  fn visit_fn(&self, ctx: &mut T, fd: &'v FnDecl, args: &'v Vec<Box<Expr>>) {
	    for ref arg in args {
	      walk_expr(self, ctx, arg);
	    }
	  }
	
	  fn visit_between(&self, ctx: &mut T, predicand: &'v Expr, begin: &'v Expr, end: &'v Expr) {
	    walk_expr(self, ctx, predicand);
	    walk_expr(self, ctx, begin);
	    walk_expr(self, ctx, end);
	  }
	  
	  fn visit_switch(&self, ctx: &mut T, cases: &'v Vec<Box<Expr>>, default_value: &'v Expr) {
	    for ref each_case in cases {
	      walk_expr(self, ctx, each_case);
	    }
	    walk_expr(self, ctx, default_value);
	  }
	  fn visit_case(&self, ctx: &mut T, condition: &'v Expr, result: &'v Expr) {
	    walk_expr(self, ctx, condition);
	    walk_expr(self, ctx, result);
	  }
	
	  fn visit_field(&self, ctx: &mut T, field: &'v String) {}
	  fn visit_const(&self, ctx: &mut T, literal: &'v Literal) {}
	}
	
	/// Default walker function for Expr Tree
	pub fn walk_expr<'v, T, V: Visitor<'v, T>>(visitor: &V, ctx: &mut T, expr: &'v Expr) {

	  match *expr.spec() {
	
	    ExprSpec::Not(ref child) => visitor.visit_not(ctx, child),
	
	    ExprSpec::IsNull(ref child, positive) => {
	      visitor.visit_is_null(ctx, child, positive)
	    }
	
	    ExprSpec::Sign(ref child, positive) => {
	      visitor.visit_sign(ctx, child, positive)
	    }
	
	    ExprSpec::Cast(ref value, ref from, ref to) => {
	      visitor.visit_cast(ctx, value, from, to)
	    }
	
	    ExprSpec::And(ref lhs, ref rhs) => visitor.visit_and(ctx, lhs, rhs),
	
	    ExprSpec::Or(ref lhs, ref rhs) => visitor.visit_or(ctx, lhs, rhs),
	
	    ExprSpec::Comp(ref op, ref lhs, ref rhs) => {
	      visitor.visit_comp(ctx, op, lhs, rhs);
	    }
	
	    ExprSpec::Arithm(ref op, ref lhs, ref rhs) => {
	      visitor.visit_arithm(ctx, op, lhs, rhs);
	    },
		    
	    ExprSpec::Fn(ref fd, ref args) => visitor.visit_fn(ctx, fd, args),
	
	    ExprSpec::Between(ref predicand, ref begin, ref end) => {
	      visitor.visit_between(ctx, predicand, begin, end)
	    },
	    
	    ExprSpec::Switch(ref cases, ref default_value) => {
	      visitor.visit_switch(ctx, cases, default_value)
	    }
	    ExprSpec::Case(ref condition, ref result) => {
	      visitor.visit_case(ctx, condition, result)
	    },
	
	    ExprSpec::Field(ref name) => visitor.visit_field(ctx, name),
	
	    ExprSpec::Const(ref datum) => visitor.visit_const(ctx, datum),
	  }
	}
	
	/// Simple visitor to walk all Expr node in a single accept function.
	/// It provides an easier way to rewrite a Expr tree.
	pub trait SimpleVisitor: Sized {
	  fn accept(&mut self, op: &Expr) {
	    accept_by_default(self, op);
	  } 
	}
	
	pub fn accept_by_default<T>(v: &mut T, e: &Expr) where T: SimpleVisitor + Sized {
	    match *e.spec() {
	      ExprSpec::Not      (ref c)               => v.accept(c),
	      ExprSpec::IsNull   (ref c, _)            => v.accept(c),
	      ExprSpec::Sign     (ref c, _)            => v.accept(c),
	      ExprSpec::Cast     (ref c, _, _)         => v.accept(c),
	      
	      ExprSpec::And      (ref l, ref r)        => { v.accept(l); v.accept(r) },
	      ExprSpec::Or       (ref l, ref r)        => { v.accept(l); v.accept(r) },
	      ExprSpec::Comp     (_, ref l, ref r)     => { v.accept(l); v.accept(r) },
	      ExprSpec::Arithm   (_, ref l, ref r)     => { v.accept(l); v.accept(r) }, 
	      
	      ExprSpec::Fn       (_, ref args)         => { for e in args.iter() { v.accept(e) } },
	      
	      ExprSpec::Between  (ref p, ref b, ref e)     => { 
	      	v.accept(p); v.accept(b); v.accept(e);
	     	},
	      
	      ExprSpec::Switch   (ref cases, ref default)  => {  
	      	for c in cases.iter() {
	      		v.accept(c);
	      	}
	      	
	      	v.accept(default);
	      },
	      ExprSpec::Case      (ref l, ref r)        => { v.accept(l); v.accept(r) },
	      
				ExprSpec::Field     (_) 	                => {},
	      ExprSpec::Const     (_)                   => {}
	    }
	  }
}