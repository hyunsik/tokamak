//! Processor
//!
//! ## Terminololgy
//! * Evaluator - a code fragment to evaluate an expression
//! * Processor - A list of evaluators
//!
//! ## Phases for Generation
//!

use std::convert::From;
use std::rc::Rc;

use common::err::{Error, Result, Void, void_ok};
use common::func::{
	NoArgFn,
	UnaryFn,
	BinaryFn,
	TrinityFn
};
use common::plugin::{
	FuncRegistry,
	TypeRegistry
};
use common::rows::{
	MiniPage,
  FMiniPage,
	Page,
	OwnedPageBuilder,
	PageId
};
use common::session::Session;
use common::types::{Ty, name};

use jit::{JitCompiler};
use jit::builder::{Builder, CastOp};
use jit::types::{self, LLVMTy};
use jit::value::{Function, Predicate, Value, ValueRef, ToValue};
use plan::expr::*;
use plan::expr::visitor::{accept_by_default, Visitor};

use driver::DriverContext;
use super::NamedSchema;

#[repr(C)]
pub struct MiniPage2 {
  ptr: *const u8,
  size: usize
}

pub type MapFunc = extern "C" fn(&mut MiniPage2) -> i32;

macro_rules! unary_codegen (
  ($name:ident, $func:ident) => (
    pub fn $name(&mut self, expr: &Expr)
    {
      let val = self.visit(expr);
      let builder = &self.builder;

      self.stack.push(builder.$func(&val));
    }
  );
);

macro_rules! bin_codegen (
  ($name:ident, $func:ident) => (
    pub fn $name(&mut self, lhs: &Expr, rhs: &Expr)
    {
      println!("before lhs");
      let lhs_val = self.visit(lhs);
      println!("before rhs");
      let rhs_val = self.visit(rhs);
      let builder = &self.builder;

      self.stack.push(builder.$func(&lhs_val, &rhs_val));
    }
  );
);

fn to_llvm_ty<'a>(jit: &'a JitCompiler, ty: &Ty) -> &'a types::Ty
{
  match *ty {
    Ty::Bool => jit.get_void_ty(),
    Ty::I8   => jit.get_void_ty(),
    Ty::I16  => jit.get_void_ty(),
    Ty::I32  => jit.get_void_ty(),
    Ty::I64  => jit.get_void_ty(),
    Ty::F32  => jit.get_void_ty(),
    Ty::F64  => jit.get_void_ty(),
    _        => panic!("not supported type: {}", ty.base())
  }
}

pub struct MapCompiler<'a>
{
  jit  : &'a JitCompiler,
  types: &'a Vec<Ty>,
	names: &'a Vec<&'a str>,
	stack: Vec<Value>,
  error: Option<Error>,
  func: Function,
  builder: Builder
}

impl<'a> MapCompiler<'a> {
  fn new(jit: &'a JitCompiler,
         fn_registry: &FuncRegistry,
		     session: &Session,
		     schema : &'a NamedSchema,) -> MapCompiler<'a> {
    let builder = jit.new_builder();
    let func = MapCompiler::generate_fn_prototype(jit, &builder);

		MapCompiler {
      jit   : jit,
			types : &schema.types,
			names : &schema.names,
			stack : Vec::new(),
			error : None,
      func: func,
      builder: builder
		}
	}

  pub fn compile(
            jit: &'a JitCompiler,
						fn_registry: &FuncRegistry,
		        session    : &'a Session,
					  schema     : &'a NamedSchema,
					  expr       : &Expr) -> Result<Rc<MapFunc>>
	{
    let mut map = MapCompiler::new(jit, fn_registry, session, schema);
    println!("before accept");
    map.accept(expr);
    println!("after accept");
    MapCompiler::generate_scalar_func(&mut map, jit);
    println!("after generate_scalar_func");
    map.ret_func(jit)
  }

  fn generate_fn_prototype(jit: &JitCompiler, bld: &Builder) -> Function {
    let minipage_ty = jit.get_ty("struct.MiniPage").unwrap();
    let minipage_ptr_ty = jit.get_pointer_ty(&minipage_ty);
    jit.create_func_prototype(
      "processor",
      &i32::llvm_ty(jit.context()),
      &[&minipage_ptr_ty],
      Some(bld))
  }

  fn generate_scalar_func(map: &mut MapCompiler, jit: &JitCompiler) {
    let func = &map.func;
    let builder = &map.builder;

    let codegen = map.stack.pop().unwrap();
    let minipage: Value = func.arg(0).into();

    let call = match jit.get_func("test") {
      Some(f) => {
        builder.create_call(&f, &[&minipage, &codegen])
      },
      _       => panic!("No such a function")
    };
    builder.create_ret(&call);
  }

  fn ret_func(&self, jit: &JitCompiler) -> Result<Rc<MapFunc>>
  {
    // dump code
    jit.dump();

    match jit.verify() {
      Ok(_) => {
        let func_ptr = unsafe { jit.get_func_ptr(&self.func).unwrap() };
        let func: MapFunc = unsafe {::std::mem::transmute(func_ptr)};
        Ok(Rc::new(func))
      }
      Err(msg) => {
        error!("{}", msg);
        Err(Error::FunctionCorrupted)
      }
    }
  }

  #[inline(always)]
  fn visit(&mut self, expr: &Expr) -> Value
  {
    self.accept(expr);
    // Stack must contain at least one Value item.
    // Otherwise, it is definitely a bug.
    self.stack.pop().unwrap()
  }

  #[inline(always)]
  fn push_value<T: ToValue>(&mut self, val: &T)
  {
    let v = val.to_value(self.jit.context());
    self.stack.push(v);
  }

  unary_codegen!(Not, create_not);

	pub fn IsNull(&self, c: &Expr)
	{
	}

	pub fn IsNotNull(&self, e: &Expr)
	{
	}

	pub fn PlusSign(&mut self, e: &Expr)
	{
    self.accept(e);
	}

  unary_codegen!(MinusSign, create_neg);

	pub fn Cast(&mut self, e: &Expr, from: &Ty, to: &Ty)
	{
    // if the both types are equivalent, just return.
    if from == to { return; }

    let cast_op = if from.is_sint() && to.is_sint() {
      if from.size_of() < to.size_of() {
        CastOp::SExt
      } else {
        CastOp::Trunc
      }
    } else if from.is_sint() && to.is_float() {
      CastOp::SIToFP
    } else if from.is_uint() && to.is_float() {
      CastOp::UIToFP
    } else if from.is_float() && to.is_sint() {
      CastOp::FPToSI
    } else if from.is_float() && to.is_uint() {
      CastOp::FPToUI
    } else if from.is_float() && to.is_float() {
      if from.size_of() < to.size_of() {
        CastOp::FPExt
      } else {
        CastOp::FPTrunc
      }
    } else {
      panic!("not supported cast from {} to {}", from.base(), to.base());
    };

    let val = self.visit(e);
    let builder = &self.builder;

    self.stack.push(builder.create_cast(cast_op, &val, to_llvm_ty(self.jit, to)));
	}

  bin_codegen!(And, create_and);
  bin_codegen!(Or,  create_or);
  bin_codegen!(Xor, create_xor);

	pub fn Cmp(&mut self, op: &CmpOp, lhs: &Expr, rhs: &Expr)
	{
    let lhs_val = self.visit(lhs);
    let rhs_val = self.visit(rhs);

    let builder = &self.builder;

    let pred = match *op {
      CmpOp::Eq => Predicate::Eq,
      CmpOp::Ne => Predicate::Ne,
      CmpOp::Lt => Predicate::Lt,
      CmpOp::Le => Predicate::Le,
      CmpOp::Gt => Predicate::Gt,
      CmpOp::Ge => Predicate::Ge
    };

    self.stack.push(builder.create_cmp(&lhs_val, &rhs_val, pred));
	}

	pub fn Arithm(&mut self, ty: &Ty, op: &ArithmOp, lhs: &Expr, rhs: &Expr)
	{
    let lhs_val = self.visit(lhs);
    let rhs_val = self.visit(rhs);

    let builder = &self.builder;

    self.stack.push(match *op {
      ArithmOp::Plus => {builder.create_add(&lhs_val, &rhs_val)}
      ArithmOp::Sub  => {builder.create_sub(&lhs_val, &rhs_val)}
      ArithmOp::Mul  => {builder.create_mul(&lhs_val, &rhs_val)}
      ArithmOp::Div  => {builder.create_div(&lhs_val, &rhs_val)}
      ArithmOp::Rem  => {builder.create_rem(&lhs_val, &rhs_val)}
    });
	}

	pub fn Func(&self, f: &FnDecl, args: &Vec<Box<Expr>>)
	{
	}

  pub fn Const(&mut self, l: &Literal)
  {
    match *l {
      Literal::U8(ref v)  => self.push_value(v),
      Literal::I8(ref v)  => self.push_value(v),
      Literal::I16(ref v) => self.push_value(v),
      Literal::I32(ref v) => self.push_value(v),
      Literal::I64(ref v) => self.push_value(v),
      Literal::F32(ref v) => self.push_value(v),
      Literal::F64(ref v) => self.push_value(v),
      _                    => unimplemented!()
    }
  }

	pub fn Field(&mut self, ty: &Ty, name: &str)
	{
		/*
    let found: Option<(usize, &Ty, &&str)>;

		found = izip!(0 .. self.types.len(), self.types, self.names)
						.find(|&(i, t, n)| *n == name);

    let eval = match found {
    	Some(f) => FieldEvaluator {idx: f.0, ty: f.1.clone()},
    	None    => panic!("no such field for {}", name)
    };*/
	}
}

impl<'a> visitor::Visitor for MapCompiler<'a>
{
	fn accept(&mut self, e: &Expr)
	{
	  match *e.kind() {
	    ExprKind::Not      (ref c)               => self.Not(c),
	    ExprKind::IsNull   (ref c)               => self.IsNull(c),
	    ExprKind::IsNotNull(ref c)               => self.IsNotNull(c),
	    ExprKind::PlusSign (ref c)               => self.PlusSign(c),
	    ExprKind::MinusSign(ref c)               => self.MinusSign(c),
	    ExprKind::Cast     (ref c, ref f, ref t) => self.Cast(c, f, t),

	    ExprKind::And      (ref l, ref r)        => self.And(l, r),
	    ExprKind::Or       (ref l, ref r)        => self.Or(l, r),
	    ExprKind::Cmp      (ref o, ref l, ref r) => self.Cmp(o, l, r),
	    ExprKind::Arithm   (ref o, ref l, ref r) => self.Arithm(e.ty(), o, l, r),


	    ExprKind::Fn     (ref f, ref args)  => self.Func(f, args),
			ExprKind::Field  (ref name)         => self.Field(e.ty(), name),
	    ExprKind::Const  (ref lit)          => self.Const(lit),
	    /*
	    ExprKind::Switch(ref cases, ref default) => {
	    	for c in cases.iter() {
	    		v.accept(c);
	   	  }

	    	v.accept(default);
	    },

	    ExprKind::Case   (ref l, ref r) => { v.accept(l); v.accept(r) },*/
	    _ => panic!("")
	  }
	}
}


pub trait Processor
{
  fn process(
    &self,
    input: &Page,
    builder: &mut OwnedPageBuilder) -> Void;
}

#[cfg(test)]
mod tests {
	use common::rows::{
		Page,
		OwnedPageBuilder,
		PageId
	};
	use common::plugin::*;
	use common::session::Session;
	use common::storage::{RandomTable, MemTable};
	use common::types::*;

  use jit::JitCompiler;

	use plan::expr::*;
	use driver::DriverContext;

	use super::super::NamedSchema;

	use super::*;

  #[test]
  pub fn codegen() {
    let plugin_mgr = PluginManager::new();
    let jit = JitCompiler::new_from_bc("../common/target/ir/common.bc").ok().unwrap();
    assert!(jit.get_ty("struct.MiniPage").is_some());
    assert!(jit.get_ty("struct.Page").is_some());

    let types: Vec<Ty> = schema!(
	    I64, // l_orderkey      bigint
	    I64, // l_partkey       bigint
	    I64, // l_suppkey       bigint
	    I32, // l_linenumber    int
	    F64, // l_quantity      double,
	    F64, // l_extendedprice double,
	    F64, // l_discount      double,
	    F64  // l_tax           double,
	    // string types are not implmeneted yet.
    );

	  let names: Vec<&str> = vec![
	  	"l_orderkey",
	  	"l_partkey",
	  	"l_suppkey",
	  	"l_linenumber",
	  	"l_quantity",
	  	"l_extendedprice",
	  	"l_discount",
	  	"l_tax"
	  ];

    let expr = Plus(I32, Const(19800401i32), Const(1i32));
    //let expr = Or(Const(4i32), Const(2i32));
    //let expr = Not(Const(0i32));
    //let expr = MinusSign(Const(7i32));
    let schema  = NamedSchema::new(&names, &types);
  	let session = Session;

    let map = MapCompiler::compile(&jit,
                                   plugin_mgr.fn_registry(),
																	 &session,
																	 &schema,
																	 &expr).ok().unwrap();

    let mut minipage = MiniPage2 {
      ptr: ::std::ptr::null(),
      size: 0
    };
    assert_eq!(map(&mut minipage), 19800401i32);
  }
}