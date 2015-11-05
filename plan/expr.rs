use common::types::{
	Ty,
	bool_ty
};

#[derive(Clone)]
pub struct Expr (pub Ty, pub ExprSpec);

impl Expr 
{
	#[inline]
	pub fn ty(&self) -> &Ty
	{
		&self.0
	}
	
	#[inline]
	pub fn spec(&self) -> &ExprSpec
	{
		&self.1
	}
}

/// Expression Specific Element
#[derive(Clone)]
pub enum ExprSpec {
  // Unary Expressions
  Not      (Box<Expr>),
  IsNull   (Box<Expr>),
  IsNotNull(Box<Expr>), 
  PlusSign (Box<Expr>),
  MinusSign(Box<Expr>),
  Cast     (Box<Expr>, Ty, Ty),

  // Binary Arithmetic Expressions
  And(Box<Expr>,Box<Expr>),
  Or(Box<Expr>,Box<Expr>),
  Comp(CompOp, Box<Expr>, Box<Expr>),
  Arithm(ArithmOp, Box<Expr>, Box<Expr>),

  // function and values
  Fn(FnDecl, Vec<Box<Expr>>),
  Field(String),
  Const(Literal),

  // condition
  Switch(Vec<Box<Expr>>, Box<Expr>), // multiple conditions, else return value
  Case  (Box<Expr>, Box<Expr>),      // condition, return value
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
  signature: String,
  ret_ty: Ty
}

impl FnDecl 
{
	pub fn ty(&self) -> &Ty
	{
		&self.ret_ty
	}
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

pub fn Not(c: Expr) -> Expr 
{
	Expr(c.ty().clone(), ExprSpec::Not(Box::new(c)))
}

pub fn IsNull(c: Expr) -> Expr 
{
	Expr(bool_ty(), ExprSpec::IsNull(Box::new(c)))
}

pub fn IsNotNull(c: Expr) -> Expr 
{
	Expr(bool_ty(), ExprSpec::IsNull(Box::new(c)))
}

pub fn Cast(c: Expr, from_ty: &Ty, to_ty: &Ty) -> Expr
{
	Expr(to_ty.clone(), ExprSpec::Cast(Box::new(c), from_ty.clone(), to_ty.clone()))
}

pub fn PlusSign(c: Expr) -> Expr
{
	Expr(c.ty().	clone(), ExprSpec::PlusSign(Box::new(c)))
}

pub fn MinusSign(c: Expr) -> Expr
{
	Expr(c.ty().clone(), ExprSpec::MinusSign(Box::new(c)))
}

pub fn And(l: Expr, r: Expr) -> Expr
{
	Expr(bool_ty(), ExprSpec::And(Box::new(l), Box::new(r)))
}

pub fn Or(l: Expr, r: Expr) -> Expr
{
	Expr(bool_ty(), ExprSpec::Or(Box::new(l), Box::new(r)))
}

pub fn Comp(op: &CompOp, l: Expr, r: Expr) -> Expr
{
	Expr(bool_ty(), ExprSpec::Comp(*op, Box::new(l), Box::new(r)))
}

pub fn Arithm(op: &ArithmOp, ret_type: &Ty, l: Expr, r: Expr) -> Expr
{
	Expr(ret_type.clone(), ExprSpec::Arithm(*op, Box::new(l), Box::new(r)))
}

pub fn Func(decl: FnDecl, args: Vec<Expr>) -> Expr
{
	Expr(decl.ty().clone(), ExprSpec::Fn(decl, to_boxed_vec(args))) 
}

pub fn Switch(cases: Vec<Expr>, default: Expr) -> Expr
{
	Expr(default.ty().clone(), ExprSpec::Switch(to_boxed_vec(cases), Box::new(default)))
}

pub fn Case(cond: Expr, result: Expr) -> Expr
{
	Expr(result.ty().clone(), ExprSpec::Case(Box::new(cond), Box::new(result)))
}

pub fn to_boxed_vec<T>(exprs: Vec<T>) -> Vec<Box<T>>
{
	exprs.into_iter().map(|e| Box::new(e)).collect::<Vec<Box<T>>>()
}

pub fn clone(e: &Box<Expr>) -> Expr
{
	*(e.clone())
}

pub fn transform_or<V, F>(v: &V, cond: bool, e: &Expr, f: F) -> Expr
		where V: visitor::TransformVisitor + Sized, 
		      F: Fn(&Expr) -> Expr
{
	if  cond {
		f(e)
	} else {
		visitor::transform_by_default(v, e)
	}
}

pub mod visitor {
	//! Visitor for Expr
	
	use common::types::Ty;
	use super::*;
	
	/// Simple visitor to walk all Expr node in a single accept function.
	/// It provides an easier way to rewrite a Expr tree.
	pub trait SimpleVisitor: Sized 
	{
	  fn accept(&mut self, e: &Expr) 
	  {
	    accept_by_default(self, e);
	  } 
	}
	
	pub fn accept_by_default<T>(v: &mut T, e: &Expr) where T: SimpleVisitor + Sized {
	  match *e.spec() {
	    ExprSpec::Not      (ref c)       => v.accept(c),
	    ExprSpec::IsNull   (ref c)       => v.accept(c),
	    ExprSpec::IsNotNull(ref c)       => v.accept(c),
	    ExprSpec::PlusSign (ref c)       => v.accept(c),
	    ExprSpec::MinusSign(ref c)       => v.accept(c),
	    ExprSpec::Cast     (ref c, _, _) => v.accept(c),
	    
	    ExprSpec::And      (ref l, ref r)    => { v.accept(l); v.accept(r) },
	    ExprSpec::Or       (ref l, ref r)    => { v.accept(l); v.accept(r) },
	    ExprSpec::Comp     (_, ref l, ref r) => { v.accept(l); v.accept(r) },
	    ExprSpec::Arithm   (_, ref l, ref r) => { v.accept(l); v.accept(r) }, 
	      
	    ExprSpec::Fn     (_, ref args)  => { for e in args.iter() { v.accept(e) } },  
			ExprSpec::Field  (_) 	          => {},
	    ExprSpec::Const  (_)            => {}
	    
	    ExprSpec::Switch(ref cases, ref default) => {  
	    	for c in cases.iter() {
	    		v.accept(c);
	   	  }
	      	
	    	v.accept(default);
	    },
	    
	    ExprSpec::Case   (ref l, ref r) => { v.accept(l); v.accept(r) },
	  }
	}
	
	/// Simple visitor to walk all Expr node in a single accept function.
	/// It provides an easier way to rewrite a Expr tree.
	pub trait TransformVisitor: Sized
  {
	  fn transform(&self, e: &Expr) -> Expr 
	  {
	    transform_by_default(self, e)
	  } 
	}
	
	pub fn transform_by_default<T>(v: &T, e: &Expr) -> Expr 
			where T: TransformVisitor + Sized
	{
		match *e.spec() {
	    ExprSpec::Not      (ref c)               => Not      (v.transform(c)),
	    ExprSpec::IsNull   (ref c)               => IsNull   (v.transform(c)),
	    ExprSpec::IsNotNull(ref c)               => IsNotNull(v.transform(c)),
	    ExprSpec::PlusSign (ref c)               => PlusSign (v.transform(c)),
	    ExprSpec::MinusSign(ref c)               => MinusSign(v.transform(c)),
	    ExprSpec::Cast     (ref c, ref f, ref t) => Cast     (v.transform(c), f, t),

	    ExprSpec::And      (ref l, ref r)        => And(v.transform(l), v.transform(r)),
	    ExprSpec::Or       (ref l, ref r)        => Or (v.transform(l), v.transform(r)),
	    ExprSpec::Comp     (ref o, ref l, ref r) => Comp(o, v.transform(l), v.transform(r)),
	    ExprSpec::Arithm   (ref o, ref l, ref r) => Arithm(o, e.ty(), v.transform(l), v.transform(r)), 
	      
	    ExprSpec::Fn       (ref f, ref args)     => { 
	    	let rargs = args.iter().map(|arg| v.transform(arg)).collect::<Vec<Expr>>();
	    	Func(f.clone(), rargs)  
    	},
			ExprSpec::Field     (_) 	                => e.clone(),
	    ExprSpec::Const     (_)                   => e.clone(),

	    ExprSpec::Switch   (ref cases, ref default)  => {  
	    	let rcases = cases.iter().map(|case| v.transform(case)).collect::<Vec<Expr>>();
	    	
	    	Switch(rcases, v.transform(default)) 
   	  }
	    ExprSpec::Case      (ref l, ref r)        => Case(v.transform(l), v.transform(r)),
	  }
	}
}