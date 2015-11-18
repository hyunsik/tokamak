use common::types::{
	Ty,
	bool_ty
};

use util::boxed::{
	ToBoxedVec,
};

#[derive(Clone)]
pub struct Expr (pub Ty, pub ExprKind);

impl Expr 
{
	#[inline]
	pub fn ty(&self) -> &Ty
	{
		&self.0
	}
	
	#[inline]
	pub fn kind(&self) -> &ExprKind
	{ 
		&self.1
	}
}

/// Expression Specific Element
#[derive(Clone)]
pub enum ExprKind {
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
  Cmp(CmpOp, Box<Expr>, Box<Expr>),
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
pub enum CmpOp {
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
  I8(i8),
  I16(i16),
  I32(i32),
  I64(i64),
  F32(f32),
  F64(f64),
  String(String)
}

macro_rules! into_literal(
	($ty:ty, $literal_kind:ident) => (
    impl Into<Literal> for $ty {
	    fn into(self) -> Literal 
	    {
	    	Literal::$literal_kind(self)
	    }
    }
  );
);

into_literal!(bool, Bool);
into_literal!(i8,   I8);
into_literal!(i16,  I16);
into_literal!(i32,  I32);
into_literal!(i64,  I64);
into_literal!(f32,  F32);
into_literal!(f64,  F64);

#[allow(non_snake_case)]
pub fn Not(c: Expr) -> Expr 
{
	Expr(c.ty().clone(), ExprKind::Not(Box::new(c)))
}

#[allow(non_snake_case)]
pub fn IsNull(c: Expr) -> Expr 
{
	Expr(bool_ty(), ExprKind::IsNull(Box::new(c)))
}

#[allow(non_snake_case)]
pub fn IsNotNull(c: Expr) -> Expr 
{
	Expr(bool_ty(), ExprKind::IsNull(Box::new(c)))
}

#[allow(non_snake_case)]
pub fn Cast(c: Expr, from_ty: &Ty, to_ty: &Ty) -> Expr
{
	Expr(to_ty.clone(), ExprKind::Cast(Box::new(c), from_ty.clone(), to_ty.clone()))
}

#[allow(non_snake_case)]
pub fn PlusSign(c: Expr) -> Expr
{
	Expr(c.ty().	clone(), ExprKind::PlusSign(Box::new(c)))
}

#[allow(non_snake_case)]
pub fn MinusSign(c: Expr) -> Expr
{
	Expr(c.ty().clone(), ExprKind::MinusSign(Box::new(c)))
}

#[allow(non_snake_case)]
pub fn And(l: Expr, r: Expr) -> Expr
{
	Expr(bool_ty(), ExprKind::And(Box::new(l), Box::new(r)))
}

#[allow(non_snake_case)]
pub fn Or(l: Expr, r: Expr) -> Expr
{
	Expr(bool_ty(), ExprKind::Or(Box::new(l), Box::new(r)))
}

#[allow(non_snake_case)]
pub fn Cmp(op: CmpOp, l: Expr, r: Expr) -> Expr
{
	Expr(bool_ty(), ExprKind::Cmp(op, Box::new(l), Box::new(r)))
}

#[allow(non_snake_case)]
pub fn Arithm(op: &ArithmOp, ret_type: &Ty, l: Expr, r: Expr) -> Expr
{
	Expr(ret_type.clone(), ExprKind::Arithm(*op, Box::new(l), Box::new(r)))
}

pub fn Subtract(ret_type: &Ty, l: Expr, r: Expr) -> Expr {
	Expr(ret_type.clone(), ExprKind::Arithm(ArithmOp::Sub, Box::new(l), Box::new(r)))
}

pub fn Mul(ret_type: &Ty, l: Expr, r: Expr) -> Expr {
	Expr(ret_type.clone(), ExprKind::Arithm(ArithmOp::Mul, Box::new(l), Box::new(r)))
}

#[allow(non_snake_case)]
pub fn Func(decl: FnDecl, args: Vec<Expr>) -> Expr
{
	Expr(decl.ty().clone(), ExprKind::Fn(decl, args.to_boxed())) 
}

#[allow(non_snake_case)]
pub fn Switch(cases: Vec<Expr>, default: Expr) -> Expr
{
	Expr(default.ty().clone(), ExprKind::Switch(cases.to_boxed(), Box::new(default)))
}

#[allow(non_snake_case)]
pub fn Case(cond: Expr, result: Expr) -> Expr
{
	Expr(result.ty().clone(), ExprKind::Case(Box::new(cond), Box::new(result)))
}

#[allow(non_snake_case)]
pub fn Const<T: Into<Literal>>(value: T) -> Expr
{
	let literal = value.into();
	let ty = match literal {
		Literal::Bool(_) => bool_ty(),
		_                => panic!("unsupported type")
	};
	
	Expr(ty, ExprKind::Const(literal))
}

#[allow(non_snake_case)]
pub fn Field(name: String, ty: Ty) -> Expr
{
	Expr(ty, ExprKind::Field(name))
}

pub fn clone(e: &Box<Expr>) -> Expr
{
	*(e.clone())
}

pub mod optimizer
{
	pub use expr_optimizer::*;
}

pub mod visitor 
{
	//! Visitor for Expr
	use super::*;
	
	/// Simple visitor to walk all Expr node in a single accept function.
	/// It provides an easier way to rewrite a Expr tree.
	pub trait Visitor: Sized 
	{
	  fn accept(&mut self, e: &Expr) 
	  {
	    accept_by_default(self, e);
	  } 
	}
	
	pub fn accept_by_default<T>(v: &mut T, e: &Expr) where T: Visitor + Sized {
	  match *e.kind() {
	    ExprKind::Not      (ref c)       => v.accept(c),
	    ExprKind::IsNull   (ref c)       => v.accept(c),
	    ExprKind::IsNotNull(ref c)       => v.accept(c),
	    ExprKind::PlusSign (ref c)       => v.accept(c),
	    ExprKind::MinusSign(ref c)       => v.accept(c),
	    ExprKind::Cast     (ref c, _, _) => v.accept(c),
	    
	    ExprKind::And      (ref l, ref r)    => { v.accept(l); v.accept(r) },
	    ExprKind::Or       (ref l, ref r)    => { v.accept(l); v.accept(r) },
	    ExprKind::Cmp      (_, ref l, ref r) => { v.accept(l); v.accept(r) },
	    ExprKind::Arithm   (_, ref l, ref r) => { v.accept(l); v.accept(r) }, 
	      
	    ExprKind::Fn     (_, ref args)  => { for e in args.iter() { v.accept(e) } },  
			ExprKind::Field  (_) 	          => {},
	    ExprKind::Const  (_)            => {}
	    
	    ExprKind::Switch(ref cases, ref default) => {  
	    	for c in cases.iter() {
	    		v.accept(c);
	   	  }
	      	
	    	v.accept(default);
	    },
	    
	    ExprKind::Case   (ref l, ref r) => { v.accept(l); v.accept(r) },
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
		match *e.kind() {
	    ExprKind::Not      (ref c)               => Not      (v.transform(c)),
	    ExprKind::IsNull   (ref c)               => IsNull   (v.transform(c)),
	    ExprKind::IsNotNull(ref c)               => IsNotNull(v.transform(c)),
	    ExprKind::PlusSign (ref c)               => PlusSign (v.transform(c)),
	    ExprKind::MinusSign(ref c)               => MinusSign(v.transform(c)),
	    ExprKind::Cast     (ref c, ref f, ref t) => Cast     (v.transform(c), f, t),

	    ExprKind::And      (ref l, ref r)        => And(v.transform(l), v.transform(r)),
	    ExprKind::Or       (ref l, ref r)        => Or (v.transform(l), v.transform(r)),
	    ExprKind::Cmp      (ref o, ref l, ref r) => Cmp(*o, v.transform(l), v.transform(r)),
	    ExprKind::Arithm   (ref o, ref l, ref r) => Arithm(o, e.ty(), v.transform(l), v.transform(r)), 
	      
	    ExprKind::Fn       (ref f, ref args)     => { 
	    	let rargs = args.iter().map(|arg| v.transform(arg)).collect::<Vec<Expr>>();
	    	Func(f.clone(), rargs)  
    	},
			ExprKind::Field     (_) 	                => e.clone(),
	    ExprKind::Const     (_)                   => e.clone(),

	    ExprKind::Switch   (ref cases, ref default)  => {  
	    	let rcases = cases.iter().map(|case| v.transform(case)).collect::<Vec<Expr>>();
	    	
	    	Switch(rcases, v.transform(default)) 
   	  }
	    ExprKind::Case      (ref l, ref r)        => Case(v.transform(l), v.transform(r)),
	  }
	}
}