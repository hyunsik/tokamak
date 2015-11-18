//! Processor
//!
//! ## Terminololgy
//! * Evaluator - a code fragment to evaluate an expression
//! * Processor - A list of evaluators
//!
//! ## Phases for Generation
//! 

use std::rc::Rc;

use common::err::{Result, Void, void_ok};
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
	Page,
	PageBuilder,
	PageId
};
use common::session::Session;
use common::types::*;
use plan::expr::*;
use plan::expr::visitor::{accept_by_default, Visitor};

use driver::DriverContext;

pub trait Processor 
{
  fn process(
    &self, 
    input: &Page, 
    builder: &mut PageBuilder) -> Void;
}

pub trait ProcessorFactory
{
	fn create(&self, ctx: &DriverContext) -> Result<Box<Processor>>;
}

pub trait Evaluator
{
	fn evaluate(&self, input: &Page, &mut PageBuilder) -> Void;
	
	fn ty(&self) -> &Ty;
}

pub trait EvaluatorFactory
{
	fn create(&self, ctx: &DriverContext) -> Result<Box<Evaluator>>;
  
  fn types(&self) -> &Ty;
}

pub struct NoArgFnEvaluator
{
	f: NoArgFn,
	output_pid: PageId
}

pub struct UnaryFnEvaluator
{
	f: UnaryFn,
	input_pid: PageId,
	output_pid: PageId
}

pub struct BinaryFnEvaluator
{
	f: BinaryFn,
	lhs_pid: PageId,
	rhs_pid: PageId,
	output_pid: PageId
}

pub struct Interpreter<'a>
{
	ty_registry: &'a TypeRegistry,
	fn_registry: &'a FuncRegistry,
	stack      : Vec<Box<EvaluatorFactory>>
}

impl<'a> Interpreter<'a>
{
	pub fn build(session: &Session,
		        ty_register: &TypeRegistry, 
					  fn_registry: &FuncRegistry,
					  expression : &Expr) -> Result<Box<EvaluatorFactory>>
	{
		unimplemented!()
  }
	
	pub fn Not(&self, c: &Expr) 
	{
	}
	
	pub fn IsNull(&self, c: &Expr) 
	{
	}
	
	pub fn IsNotNull(&self, c: &Expr) 
	{
	}
	
	pub fn PlusSign(&self, c: &Expr) 
	{
	}
	
	pub fn MinusSign(&self, c: &Expr) 
	{
	}
	
	pub fn Cast(&self, c: &Expr, f: &Ty, t: &Ty) 
	{
	}
	
	pub fn And(&self, lhs: &Expr, rhs: &Expr) 
	{
	}
	
	pub fn Or(&self, lhs: &Expr, rhs: &Expr) 
	{
	}
	
	pub fn Cmp(&self, op: &CmpOp, lhs: &Expr, rhs: &Expr) 
	{
	}
	
	pub fn Arithm(&self, op: &ArithmOp, lhs: &Expr, rhs: &Expr) 
	{
	}
	
	pub fn Func(&self, f: &FnDecl, args: &Vec<Box<Expr>>)
	{
	}
}

pub struct UnaryEvaluatorFactory
{
	f: EvaluatorFactory
}

impl<'a> visitor::Visitor for Interpreter<'a> 
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
	    ExprKind::Or       (ref l, ref r)        => self.And(l, r),
	    ExprKind::Cmp      (ref o, ref l, ref r) => self.Cmp(o, l, r),
	    ExprKind::Arithm   (ref o, ref l, ref r) => self.Arithm(o, l, r), 

	      
	    ExprKind::Fn     (ref f, ref args)  => self.Func(f, args),  
			ExprKind::Field  (_) 	          => {},
			/*
	    ExprKind::Const  (_)            => {}
	    
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

pub struct InterpreterProcessorFactory
{
	factories: Vec<Box<EvaluatorFactory>>
}

impl InterpreterProcessorFactory
{
	pub fn new(exprs: &Vec<Box<Expr>>) -> Box<ProcessorFactory>
	{
		Box::new(InterpreterProcessorFactory {
  		factories: Vec::new()
		}) 
	}
}

impl ProcessorFactory for InterpreterProcessorFactory
{
	fn create(&self, ctx: &DriverContext) -> Result<Box<Processor>>
	{
		let evals = try!(self.factories
									.iter()
									.map(|f| f.create(ctx))
									.collect::<Result<Vec<Box<Evaluator>>>>());
		
		Ok(Box::new(InterpreterProcessor {
			evals: evals
		}))
	}
}

pub struct InterpreterProcessor
{
	evals: Vec<Box<Evaluator>>
}

impl Processor for InterpreterProcessor
{
	fn process(
    &self, 
    input: &Page, 
    builder: &mut PageBuilder) -> Void 
	{
  	for e in self.evals.iter() {
  		try!(e.evaluate(input, builder));
  	}
  	
  	void_ok
  }
}

#[cfg(test)]
mod tests {
	use common::rows::{
		Page,
		PageBuilder,
		PageId
	};
	use common::plugin::*;
	use common::session::Session;
	use common::storage::{RandomTable, MemTable};
	use common::types::*;
	
	use plan::expr::*;
	use driver::DriverContext;
	
	use super::*;
	
	#[test]
	pub fn tpch_q1() {
		let plugin_mgr = PluginManager::new();	
		
		/*
			l_orderkey long,
			l_partkey long,
			l_suppkey long,
			l_linenumber int,
			l_quantity double,
			l_extendedprice double,
			l_discount double,
			l_tax double,
			l_returnflag string,
			l_linestatus string,
			l_shipdate string,
			l_commitdate string,
			l_receiptdate string,
			l_shipinstruct string,
			l_shipmode string,
			l_comment string
		*/
		let types: Vec<Ty> = vec![
	    i64_ty(), // l_orderkey      bigint
	    i64_ty(), // l_partkey       bigint
	    i64_ty(), // l_suppkey       bigint
	    i32_ty(), // l_linenumber    int
	    f64_ty(), // l_quantity      double,
	    f64_ty(), // l_extendedprice double,
	    f64_ty(), // l_discount      double,
	    f64_ty(), // l_tax           double,
	    // string types are not implmeneted yet.	    
	  ];

	  let sum_disc_price = 
	  	Mul(
	  		&f64_ty(), Field(&f64_ty(), "l_extendedprice"), Subtract(&f64_ty(), Const(1), Field(&f64_ty(), "l_discount")));
	  	
  	let sum_charge = 
	  	Mul(&f64_ty(), sum_disc_price.clone(), Plus(&f64_ty(), Const(1), Field(&f64_ty(), "l_tax")));	   
	  
	  
	  let exprs = vec![
	  	Box::new(sum_disc_price), 
	  	Box::new(sum_charge)
  	];
		let factory = InterpreterProcessorFactory::new(&exprs);
		let drv_ctx = DriverContext::new(plugin_mgr.ty_registry(), plugin_mgr.fn_registry());
		let processor = factory.create(&drv_ctx).ok().unwrap();
		
		let session    = Session;
		let mut input  = RandomTable::new(&session, &types, 1024);
		
		let output_tys = exprs.iter()
											.map(|e| e.ty().clone())
											.collect::<Vec<Ty>>();
		let mut output = MemTable::new(&session, &output_tys, &vec!["x","y"]);
		
		let mut builder = PageBuilder::new(&output_tys);
		
		
		loop { 
		  let mut read_page = input.next().unwrap();
		  
		  if read_page.value_count() == 0 {
		  	break;
		  }
		  
		  processor.process(read_page, &mut builder);
		  output.write(builder.build(read_page.value_count()));
		  builder.reset();
		}
	}
}