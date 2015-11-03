use std::rc::Rc;

use common::err::Result;
use common::func::{
	NoArgFn,
	UnaryFn,
	BinaryFn,
	TrinityFn
};
use common::rows::{
	MiniPageWriter,
	PageId
};
use plan::expr::*;

use driver::DriverContext;
use super::Processor;

pub trait ProcessorFactory
{
	fn create(&self, ctx: &DriverContext) -> Result<Box<Processor>>;
}

pub struct InterpreterProcessorFactory
{
	evals: Vec<Box<Evaluator>>
}

pub trait Evaluator
{
	fn evaluate(&self, &mut MiniPageWriter);
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

impl InterpreterProcessorFactory
{
	pub fn new(exprs: Vec<Box<Expr>>) -> Box<ProcessorFactory>
	{
		Box::new(InterpreterProcessorFactory {
  		evals: Vec::new()
		}) 
	}
}

impl ProcessorFactory for InterpreterProcessorFactory
{
	fn create(&self, ctx: &DriverContext) -> Result<Box<Processor>>
	{
		unimplemented!()
	}
}