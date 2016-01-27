//! Processor
//!
//! ## Terminololgy
//! * Evaluator - a code fragment to evaluate an expression
//! * Processor - A list of evaluators
//!
//! ## Phases for Generation
//!

use std::collections::HashMap;
use std::convert::From;
use std::rc::Rc;

use common::err::{Error, Result, Void, void_ok};
use common::plugin::{
	FuncRegistry,
	TypeRegistry
};
use common::page::{
  c_api,
	Chunk,
	Page,
  ROWBATCH_SIZE
};
use common::session::Session;
use common::types::{Ty, name};

use jit::{JitCompiler, LLVMContextRef};
use jit::block::BasicBlock;
use jit::builder::{Builder, CastOp};
use jit::types::{self, LLVMTy};
use jit::value::{Arg, Function, Predicate, Value, ValueRef, ToValue};
use plan::expr::*;
use plan::expr::visitor::{accept_by_default, Visitor};

use driver::DriverContext;
use super::NamedSchema;

/// selected rows IDs
pub type SelectedList = *const usize;
/// (input rows, output rows, selected rows IDs, row num)
pub type MapFunc = extern "C" fn(&Page, &mut Page, SelectedList, usize);
/// (input rows, input selected rows IDs, input selected count, output selected rows IDs) -> output selected count
pub type SelFunc = extern "C" fn(&Page, SelectedList, usize, SelectedList) -> usize;

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
      let lhs_val = self.visit(lhs);
      let rhs_val = self.visit(rhs);
      let builder = &self.builder;

      self.stack.push(builder.$func(&lhs_val, &rhs_val));
    }
  );
);

fn to_llvm_ty<'a>(jit: &'a JitCompiler, ty: &Ty) -> &'a types::Ty
{
  match *ty {
    Ty::Bool => jit.get_bool_ty(),
    Ty::I8   => jit.get_i8_ty(),
    Ty::I16  => jit.get_i16_ty(),
    Ty::I32  => jit.get_i32_ty(),
    Ty::I64  => jit.get_i64_ty(),
    Ty::F32  => jit.get_f32_ty(),
    Ty::F64  => jit.get_f64_ty(),
    _        => panic!("not supported type: {}", ty.base())
  }
}

pub trait CodeGenContext<'a> {
  fn jit(&self) -> &'a JitCompiler;
  fn context(&self) -> LLVMContextRef;
  fn fn_registry(&self) -> &'a FuncRegistry;
  fn session(&self) -> &'a Session;
  fn schema(&self) -> &'a HashMap<&'a str, (usize, &'a Ty)>;
}

pub struct MapCompiler<'a> {
  jit: &'a JitCompiler,
  ctx: LLVMContextRef,
  fn_reg: &'a FuncRegistry,
  sess: &'a Session,
  schema: &'a HashMap<&'a str, (usize, &'a Ty)>,

  // cache
  get_chunk_fn: Function,
  zero: Value,
  one : Value,
  rowbatch_size: Value
}

impl<'a> CodeGenContext<'a> for MapCompiler<'a> {
  fn jit(&self) -> &'a JitCompiler { self.jit }
  fn context(&self) -> LLVMContextRef { self.ctx }
  fn fn_registry(&self) -> &'a FuncRegistry { self.fn_reg }
  fn session(&self) -> &'a Session { self.sess }
  fn schema(&self) -> &'a HashMap<&'a str, (usize, &'a Ty)> { self.schema }
}

impl<'a> MapCompiler<'a> {

  pub fn new(jit  : &'a JitCompiler,
            fn_reg: &'a FuncRegistry,
            sess  : &'a Session,
            schema: &'a HashMap<&'a str, (usize, &'a Ty)>) -> MapCompiler<'a> {

    MapCompiler {
      jit   : jit,
      ctx   : jit.context(),
      fn_reg: fn_reg,
      sess  : sess,
      schema: schema,

      get_chunk_fn : jit.get_func(c_api::FN_GET_CHUNK).unwrap(),
      zero         : jit.get_const(0usize),
      one          : jit.get_const(1usize),
      rowbatch_size: jit.get_const(ROWBATCH_SIZE)
    }
  }

  fn create_columns_accessors(&self, bld: &Builder, in_page: &Value, num: usize) -> Vec<Value> {
    (0..num)
      .map(|idx| bld.create_call(&self.get_chunk_fn, &[&in_page, &idx.to_value(self.ctx)]))
      .collect::<Vec<Value>>()
  }

  fn write_const_values(&'a self, bld: &'a Builder, func: &Function, exprs: &[&Expr]) {

    let out_page: &Value = &func.arg(1).into();
    let const_exprs = izip!(0..exprs.len(), exprs)
      .filter(|e| match *e.1.kind() {ExprKind::Const(_) => true, _ => false});

    for (out_idx, e) in const_exprs {
      let codegen = ExprCompiler::compile(self, bld, None, Some(&self.zero), e).ok().unwrap();

      self.write_value(
        bld,
        e.ty(),
        out_page,
        &self.jit.get_const(out_idx),
        &self.zero,
        &codegen);
    }
  }

  fn write_nonconst_values(&'a self,
      bld       : &'a Builder,
      func      : &Function,
      column_num: usize,
      exprs     : &[&Expr]) {

    let in_page : &Value = &func.arg(0).into();
    let out_page: &Value = &func.arg(1).into();

    // generate access variables for all chunks of input page
    let mut column_chunks: Vec<Value> = self.create_columns_accessors(bld, in_page, column_num);

    let mut eval_entry = func.append("eval_entry");
    let mut block_list: Vec<BasicBlock> = Vec::new();

    let nonconst_exprs =
      izip!(0..exprs.len(), exprs)
      .filter(|e| match *e.1.kind() {ExprKind::Const(_) => false, _ => true});

    for (out_idx, e) in nonconst_exprs {
      let loop_cond_bb    = func.append("loop_cond");
      let loop_body_bb    = func.append("loop_body");
      let next_eval_entry = func.append("eval_entry"); // next loop entry

      let loop_builder = self.jit.new_builder();

      // loop entry
      loop_builder.position_at_end(&eval_entry);
      let row_idx_ptr = loop_builder.create_alloca(self.jit.get_u64_ty());
      loop_builder.create_store(&self.zero, &row_idx_ptr);
      loop_builder.create_br(&loop_cond_bb);

      // loop condition
      loop_builder.position_at_end(&loop_cond_bb);
      let row_idx = loop_builder.create_load(&row_idx_ptr);
      let loop_cond = loop_builder.create_ucmp(&row_idx, &self.rowbatch_size, Predicate::Lt);
      loop_builder.create_cond_br(&loop_cond, &loop_body_bb, &next_eval_entry);

      // loop body
      loop_builder.position_at_end(&loop_body_bb);
      let mut codegen = ExprCompiler::compile(self,
        &loop_builder,
        Some(&column_chunks),
        Some(&row_idx),
        e).ok().unwrap();

      self.write_value(
        &loop_builder,
        e.ty(),
        out_page,
        &self.jit.get_const(out_idx),
        &row_idx,
        &codegen);

      let add_row_idx = loop_builder.create_add(&row_idx, &self.one);
      loop_builder.create_store(&add_row_idx, &row_idx_ptr);
      loop_builder.create_br(&loop_cond_bb);

      block_list.push(eval_entry);
      eval_entry = next_eval_entry;
    }

    if !block_list.is_empty() {
      bld.create_br(&block_list[0]);
    } else {
      bld.create_br(&eval_entry);
    }

    bld.position_at_end(&eval_entry);
  }

  pub fn compile(jit   : &JitCompiler,
                 fn_reg: &FuncRegistry,
                 sess  : &Session,
                 schema: &NamedSchema,
                 exprs : &[&Expr]) -> Result<Rc<MapFunc>> {

    let ctx = jit.context();
    let builder = &jit.new_builder();

    let schema_map = schema.to_map();
    let mapc = MapCompiler::new(jit, fn_reg, sess, &schema_map);
    let func = &mapc.create_fn_prototype(builder);

    let sel_list: &Value = &func.arg(2).into();
    let row_num : &Value = &func.arg(3).into();

    mapc.write_const_values(builder, func, exprs);
    mapc.write_nonconst_values(builder, func, schema.types.len(), exprs);
    builder.create_ret_void();

    // dump code
    jit.dump();
    Ok(mapc.ret_func(&func))
  }

  fn write_value(&self,
                 builder   : &Builder,
                 output_ty : &Ty,
                 out_page  : &Value,
                 output_idx: &Value,
                 row_idx   : &Value,
                 output_val: &Value) {
    let fn_name = match *output_ty {
      Ty::Bool => "write_i8_raw",
      Ty::I8   => "write_i8_raw",
      Ty::I16  => "write_i16_raw",
      Ty::I32  => "write_i32_raw",
      Ty::I64  => "write_i64_raw",
      Ty::F32  => "write_f32_raw",
      Ty::F64  => "write_f64_raw",
      _        => panic!("not supported type")
    };

    let out_chunk = builder.create_call(&self.get_chunk_fn, &[out_page, output_idx]);
    let call = match self.jit.get_func(fn_name) {
      Some(write_fn) => builder.create_call(&write_fn, &[&out_chunk, row_idx, output_val]),
      _       => panic!("No such a function")
    };
  }

  fn create_fn_prototype(&self, bld: &Builder) -> Function {
    let jit = self.jit;

    // Prototype: (&Page, &mut Page, SelectedList, usize) == (*Page, *Page, *const usize, usize)
    let page_ty = jit.get_ty("struct.Page").unwrap().pointer_ty(); // *const Page (== &Page)
    let sellist_ty = jit.get_i64_ty().pointer_ty(); // *const usize
    let usize_ty = jit.get_i64_ty(); // usize

    jit.create_func_prototype(
      "processor",
      &i32::llvm_ty(jit.context()),
      &[&page_ty, &page_ty, &sellist_ty, &usize_ty],
      Some(bld))
  }

  fn verify(&self) -> Result<()> {
    match self.jit.verify() {
      Ok(_) => Ok(()),
      Err(msg) => {
        error!("{}", msg);
        Err(Error::CorruptedFunction(msg.to_owned()))
      }
    }
  }

 fn ret_func(&self, func: &Function) -> Rc<MapFunc> {
    let func_ptr = unsafe { self.jit.get_func_ptr(func).unwrap() };
    let func = unsafe {::std::mem::transmute(func_ptr)};
    Rc::new(func)
  }
}

/// Compiler for Columnar Evaluator of Expression
pub struct ExprCompiler<'a, 'b>
{
  gen_ctx: &'a CodeGenContext<'a>,
  builder: &'b Builder,                  // LLVM IRBuilder
	stack  : Vec<Value>,                   // LLVM values stack temporarily used for code generation
  error  : Option<Error>,                // Code generation error

  input_vecs: Option<&'b Vec<Value>>,    // LLVM values to represent vector pointers
  row_idx   : Option<&'b Value>,         // LLVM values to represent row index
}

impl<'a, 'b> CodeGenContext<'a> for ExprCompiler<'a, 'b> {
  fn jit(&self) -> &'a JitCompiler { self.gen_ctx.jit() }
  fn context(&self) -> LLVMContextRef { self.gen_ctx.context() }
  fn fn_registry(&self) -> &'a FuncRegistry { self.gen_ctx.fn_registry() }
  fn session(&self) -> &'a Session { self.gen_ctx.session() }
  fn schema(&self) -> &'a HashMap<&'a str, (usize, &'a Ty)> { self.gen_ctx.schema() }
}

impl<'a, 'b> ExprCompiler<'a, 'b> {

  fn new(gen_ctx     : &'a CodeGenContext<'a>,
         builder     : &'b Builder,
         input_vecs  : Option<&'b Vec<Value>>,
         row_idx     : Option<&'b Value>) -> ExprCompiler<'a, 'b> {

		ExprCompiler {
      gen_ctx   : gen_ctx,
			stack     : Vec::new(),
			error     : None,
      builder   : builder,
      input_vecs: input_vecs,
      row_idx   : row_idx
		}
	} 

  pub fn compile(
         gen_ctx    : &'a CodeGenContext<'a>,
         bld        : &'b Builder,
         input_vecs : Option<&'b Vec<Value>>,
         row_idx    : Option<&'b Value>,
         expr: &Expr) -> Result<Value> {

    let mut exprc = ExprCompiler::new(gen_ctx, bld, input_vecs, row_idx);
    // visit a expression tree
    exprc.accept(expr);
    match exprc.stack.pop() {
      Some(v) => Ok(v),
      None    => Err(Error::CorruptedFunction("empty value stack".to_string()))
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
    let v = val.to_value(self.context());
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

    let jit = self.jit();
    self.stack.push(builder.create_cast(cast_op, &val, to_llvm_ty(jit, to)));
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
    let found :&(usize, &Ty) = self.schema()
      .get(name).expect(&format!("Field '{}' does not exist", name));
    let column_chunk = &self.input_vecs.unwrap()[found.0];
      
    debug_assert_eq!(ty, found.1);

    let call = match self.jit().get_func(c_api::fn_name_of_read_raw(ty)) {
      Some(read_fn) => self.builder.create_call(&read_fn, &[column_chunk, &self.row_idx.unwrap()]),
      _       => panic!("No such a function")
    };

    self.stack.push(call);
	}
}

impl<'a, 'b> visitor::Visitor for ExprCompiler<'a, 'b>
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
    output: &mut Page) -> Void;
}

#[cfg(test)]
mod tests {
	use common::page::{
    c_api,
    ROWBATCH_SIZE,
		Page,
		Chunk
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

  pub fn fill_page(p: &Page) {
    unsafe {
      for idx in 0..ROWBATCH_SIZE {
        c_api::write_i64_raw(p.chunk(0), idx, idx as i64);
        c_api::write_i64_raw(p.chunk(1), idx, idx as i64);
      }
    }
  }

  #[test]
  pub fn codegen() {
    let plugin_mgr = PluginManager::new();
    let jit = JitCompiler::new_from_bc("../common/target/ir/common.bc").ok().unwrap();
    assert!(jit.get_ty("struct.Chunk").is_some());
    assert!(jit.get_ty("struct.Page").is_some());

    let types = &[
	    I64, // l_orderkey      bigint
	    I64, // l_partkey       bigint
    ];

	  let names = &[
	  	"l_orderkey",
	  	"l_partkey",
    ];

    let expr1 = Field(I64, "l_orderkey");
    let expr2 = Field(I64, "l_partkey");
    let expr3 = Plus(I64, expr1.clone(), expr2.clone());

    let schema  = NamedSchema::new(names, types);
  	let session = Session;

    let map = MapCompiler::compile(&jit,
                          plugin_mgr.fn_registry(),
                          &session,
                          &schema,
                          &[&expr1, &expr2, &expr3]).ok().unwrap();

    let in_page = Page::new(schema.types, None);
    fill_page(&in_page);

    let mut out_page =  Page::new(&[I64, I64, I64], None);
    let sellist: [usize; ROWBATCH_SIZE] = unsafe { ::std::mem::uninitialized() };

    // map is the jit compiled function.
    map(&in_page, &mut out_page, sellist.as_ptr(), ROWBATCH_SIZE);

    unsafe {
      for idx in 0..ROWBATCH_SIZE {
        let lhs_val = c_api::read_i64_raw(in_page.chunk(0), idx);
        let rhs_val = c_api::read_i64_raw(in_page.chunk(1), idx);
        assert_eq!(lhs_val, c_api::read_i64_raw(out_page.chunk(0), idx));
        assert_eq!(rhs_val, c_api::read_i64_raw(out_page.chunk(1), idx));
        assert_eq!((lhs_val + rhs_val) as i64, c_api::read_i64_raw(out_page.chunk(2), idx));
      }
    }
  }

  #[test]
  pub fn tpch1() {
    let plugin_mgr = PluginManager::new();
    let jit = JitCompiler::new_from_bc("../common/target/ir/common.bc").ok().unwrap();
    assert!(jit.get_ty("struct.Chunk").is_some());
    assert!(jit.get_ty("struct.Page").is_some());

    let types = &[
	    I64, // l_orderkey      bigint
	    I64, // l_partkey       bigint
	    I64, // l_suppkey       bigint
	    I32, // l_linenumber    int
	    F64, // l_quantity      double,
	    F64, // l_extendedprice double,
	    F64, // l_discount      double,
	    F64  // l_tax           double,
	    // string types are not implmeneted yet.
    ];

	  let names = &[
	  	"l_orderkey",
	  	"l_partkey",
	  	"l_suppkey",
	  	"l_linenumber",
	  	"l_quantity",
	  	"l_extendedprice",
	  	"l_discount",
	  	"l_tax"
    ];

    // l_extendedprice*(1-l_discount)
    let expr1 = Mul(F64, Field(F64, "l_extendedprice"), Subtract(F64, Const(1.0f64), Field(F64, "l_discount")));
    //let expr1 = Const(19800401i32);
    //let expr2 = Const(19840115i32);
    //let expr1 = Plus(I32, Const(19800401i32), Const(1i32));
    //let expr2 = MinusSign(Const(7i32));
    //let expr = Or(Const(4i32), Const(2i32));
    //let expr = Not(Const(0i32));
    let schema  = &NamedSchema::new(names, types);
  	let session = Session;

    // let map = ExprCompiler::compile(&jit,
    //                                plugin_mgr.fn_registry(),
		// 															 &session,
		// 															 &schema,
		// 															 &expr).ok().unwrap();
    let map = MapCompiler::compile(&jit,
                          plugin_mgr.fn_registry(),
                          &session,
                          schema,
                          &[&expr1]).ok().unwrap();

    let in_page = Page::new(&schema.types, None);
    let mut out_page =  Page::new(&[F64], None);
    let sellist: [usize; ROWBATCH_SIZE] = unsafe { ::std::mem::uninitialized() };

    // map is the jit compiled function.
    map(&in_page, &mut out_page, sellist.as_ptr(), ROWBATCH_SIZE);
    unsafe {
      assert_eq!(0f64, c_api::read_f64_raw(out_page.chunk(0), 0));
    }
  }
}