use std::marker;

use common::err::Result;
use common::plugin::{FuncRegistry, TypeRegistry};
use common::rows::Page;

use task::TaskSource;

use super::{Executor, ExecutorFactory};

pub struct Driver<'a>
{
	ctx : &'a DriverContext<'a>,
	root_exec: Box<Executor>
}

impl<'a> Driver<'a>
{
  pub fn update_source(&self, source: TaskSource) {}
  
  pub fn process(&mut self) -> Result<&Page> 
  {
  	self.root_exec.next()
  }
  
  pub fn close(&mut self)
  {
  	self.root_exec.close();
  }
}

pub struct DriverFactory<'a>
{
  pub is_input : bool,
  pub is_output: bool, 
  source_ids   : Vec<String>,
  factory      : Box<ExecutorFactory>,
  marker       : marker::PhantomData<&'a ()>
}

impl<'a> DriverFactory<'a> {
	pub fn new(
		is_input : bool, 
		is_output: bool, 
		factory  : Box<ExecutorFactory>) -> DriverFactory<'a> 
	{
		DriverFactory {
			is_input   : is_input,
			is_output  : is_output,
			source_ids : Vec::new(),
			factory    : factory,
			marker     : marker::PhantomData
		}
	}
	
	pub fn create_driver(&self, ctx: &'a DriverContext) -> Driver<'a>
  {
  	let root_exec = self.factory.create(ctx).unwrap();
  	
    Driver {
    	ctx: ctx,
    	root_exec: root_exec
    }
  }
}

/// Driver Specific Context and Job and Node Context
pub struct DriverContext<'a>
{
	ty_registry: &'a TypeRegistry,
	fn_registry: &'a FuncRegistry 
}

impl<'a> DriverContext<'a>
{
	pub fn new(ty_registry: &'a TypeRegistry, 
						 fn_registry: &'a FuncRegistry) -> DriverContext<'a>
	{
		DriverContext {
			ty_registry: ty_registry,
			fn_registry: fn_registry 
		}
	}
}