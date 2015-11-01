use std::marker;

use common::err::Result;
use common::rows::Page;

use task::TaskSource;

use super::{Executor, ExecutorFactory};

pub struct Driver<'a>
{
	ctx : &'a DriverContext,
	execs: Vec<Box<Executor>>
}

impl<'a> Driver<'a>
{
  pub fn update_source(&self, source: TaskSource) {}
  
  pub fn process(&mut self) -> Result<&Page> 
  {
  	if self.execs.len() == 1 {
  		unsafe {self.execs.get_unchecked_mut(0).next()}
  	} else {
  		unimplemented!()
  	}
  }
  
  pub fn close(&mut self)
  {
  }
}

pub struct DriverFactory<'a>
{
  pub is_input : bool,
  pub is_output: bool, 
  source_ids   : Vec<String>,
  factories    : Vec<Box<ExecutorFactory>>,
  marker       : marker::PhantomData<&'a ()>
}

impl<'a> DriverFactory<'a> {
	pub fn new(
		is_input: bool, 
		is_output: bool, 
		factories: Vec<Box<ExecutorFactory>>) -> DriverFactory<'a> 
	{
		DriverFactory {
			is_input   : is_input,
			is_output  : is_output,
			source_ids : Vec::new(),
			factories  : factories,
			marker     : marker::PhantomData
		}
	}
	
	pub fn create_driver(&self, ctx: &'a DriverContext) -> Driver<'a>
  {
  	let execs = self.factories
  		.iter()
  		.map(|f| f.create(ctx).unwrap())
  		.collect::<Vec<Box<Executor>>>();
  	
    Driver {
    	ctx: ctx,
    	execs: execs
    }
  }
}

pub struct DriverContext;