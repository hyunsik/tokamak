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
	input_types: &'a Vec<Ty>,
	input_names: &'a Vec<&'a str>,
	stack      : Vec<Box<EvaluatorFactory>>
}

impl<'a> Interpreter<'a>
{
	fn new(session: &Session, 
		     input_types: &'a Vec<Ty>, 
		     input_names: &'a Vec<&'a str>) -> Interpreter<'a> {
		Interpreter {
			input_types: input_types,
			input_names: input_names,
			stack      : Vec::new()
		}
	}
	pub fn build(session:  &'a Session,     
					  input_types: &'a Vec<Ty>,
					  input_names: &'a Vec<&'a str>, 
					  expression : &Expr) -> Result<Box<EvaluatorFactory>>
	{
		let mut interpreter = Interpreter::new(session, input_types, input_names);
		interpreter.accept(expression);
		
		match interpreter.stack.len() {
			len if len == 1 => Ok(interpreter.stack.pop().unwrap()),
			len if len > 1  => panic!("more than one stack item still remains in interpreter"),
			_               => panic!("no more stack item in interpreter")
		}
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

pub type Schema<'a, 'b> = (&'a Vec<Ty>, &'a Vec<&'b str>);

pub struct InterpreterProcessorFactory
{
	factories: Vec<Box<EvaluatorFactory>>
}

impl InterpreterProcessorFactory
{
	pub fn new(session: &Session, 
		         schema: &Schema, 
		         exprs: &Vec<Box<Expr>>) -> Result<Box<ProcessorFactory>>
	{
		let factories = try!(
			exprs.iter()
		       .map(|e| Interpreter::build(session, schema.0, schema.1, e))
		       .collect::<Result<Vec<Box<EvaluatorFactory>>>>()
    );
			
		Ok(Box::new(InterpreterProcessorFactory { factories: factories }))
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
		let tb_types: Vec<Ty> = vec![
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
	  
	  let tb_field_names: Vec<&str> = vec![
	  	"l_orderkey",
	  	"l_partkey",
	  	"l_suppkey",
	  	"l_linenumber",
	  	"l_quantity",
	  	"l_extendedprice",
	  	"l_discount",
	  	"l_tax"
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
  	
  	let schema = (&tb_types, &tb_field_names);
  	let session    = Session;
  	
		let factory = InterpreterProcessorFactory::new(&session, &schema, &exprs).ok().unwrap();
		let drv_ctx = DriverContext::new(plugin_mgr.ty_registry(), plugin_mgr.fn_registry());
		let processor = factory.create(&drv_ctx).ok().unwrap();
		
		let mut input  = RandomTable::new(&session, &tb_types, 1024);
		
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
		
		assert_eq!(1024, output.row_num());
	}
}