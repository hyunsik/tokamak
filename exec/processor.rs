//! Processor
//!
//! ## Terminololgy
//! * Evaluator - a code fragment to evaluate an expression
//! * Processor - A list of evaluators
//!
//! ## Phases for Generation
//! 

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

pub trait Evaluator
{
	fn evaluate<'p>(&self, input: &'p Page) -> Result<&'p MiniPage>;
	
	fn ty(&self) -> &Ty;
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

#[derive(Clone)]
pub struct FieldEvaluator 
{
	idx: usize,
	ty:  Ty
}

impl Evaluator for FieldEvaluator
{
	fn evaluate<'p>(&self, input: &'p Page) -> Result<&'p MiniPage> 
	{
		Ok(input.minipage(self.idx))
	}
	
	fn ty(&self) -> &Ty 
	{
		&self.ty
	}
}

pub struct Interpreter<'a>
{
	types: &'a Vec<Ty>,
	names: &'a Vec<&'a str>,
	stack: Vec<Box<Evaluator>>,
	error: Option<Error>
}

impl<'a> Interpreter<'a>
{
	fn new(session: &Session, 
		     types: &'a Vec<Ty>, 
		     names: &'a Vec<&'a str>) -> Interpreter<'a> {
		Interpreter {
			types : types,
			names : names,
			stack : Vec::new(),
			error : None
		}
	}
		     
	pub fn build(session:  &'a Session,     
					  input_types: &'a Vec<Ty>,
					  input_names: &'a Vec<&'a str>, 
					  expression : &Expr) -> Result<Box<Evaluator>>
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
	
	pub fn Field(&mut self, ty: &Ty, name: &str)
	{
		let found: Option<(usize, &Ty, &&str)>;
		
		found = izip!(0 .. self.types.len(), self.types, self.names)
						.find(|&(i, t, n)| *n == name);
    
    let eval = match found {
    	Some(f) => FieldEvaluator {idx: f.0, ty: f.1.clone()},
    	None    => panic!("no such field for {}", name)
    };
    						
		self.stack.push(Box::new(eval));
	}
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
			ExprKind::Field  (ref name)         => self.Field(e.ty(), name),
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

pub struct InterpreterProcessor
{
	evals: Vec<Box<Evaluator>>
}

impl InterpreterProcessor
{
	pub fn new(session: &Session, 
		         schema : &Schema, 
		         exprs  : &Vec<Box<Expr>>) -> Result<InterpreterProcessor>
	{
		let evals = try!(
			exprs.iter()
		       .map(|e| Interpreter::build(session, schema.0, schema.1, e))
		       .collect::<Result<Vec<Box<Evaluator>>>>()
    );
		
		Ok(InterpreterProcessor { evals: evals })			
	}
} 

impl Processor for InterpreterProcessor
{
	fn process(
    &self, 
    input: &Page, 
    builder: &mut PageBuilder) -> Void 
	{
  	for (w, e) in izip!(builder.iter_mut(), self.evals.iter()) {
  		try!(e.evaluate(input));
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

		/*
	  let sum_disc_price = 
	  	Mul(
	  		&f64_ty(), Field(&f64_ty(), "l_extendedprice"), Subtract(&f64_ty(), Const(1), Field(&f64_ty(), "l_discount")));
	  	
  	let sum_charge = 
	  	Mul(&f64_ty(), sum_disc_price.clone(), Plus(&f64_ty(), Const(1), Field(&f64_ty(), "l_tax")));*/
		
		let l_quantity = Field(&f64_ty(), "l_quantity");	  
	  
	  let exprs = vec![
//	  	Box::new(sum_disc_price), 
//	  	Box::new(sum_charge)
				Box::new(l_quantity)
  	];
  	
  	let schema = (&tb_types, &tb_field_names);
  	let session    = Session;
  	
		let processor = InterpreterProcessor::new(&session, &schema, &exprs).ok().unwrap();
		let mut input  = RandomTable::new(&session, &tb_types, 1024);
		
		let output_tys = exprs.iter()
											.map(|e| e.ty().clone())
											.collect::<Vec<Ty>>();
		let mut output = MemTable::new(&session, &output_tys, &vec!["x"]);
		
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
		
		assert_eq!(1, output.col_num());
		assert_eq!(1024, output.row_num());
		
		for x in output.reader() {
			let r: (f64) = x.ok().unwrap();
			println!("{}", r);
		}
	}
}