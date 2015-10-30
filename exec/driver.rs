use std::marker;

use common::err::Result;
use common::rows::Page;

use task::TaskSource;

use super::Executor;

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
  pub input_driver: bool,
  pub output_driver: bool, 
  source_ids: Vec<String>,
  factory: Vec<Box<DriverFactory<'a>>>,
  marker: marker::PhantomData<&'a ()>
}

pub struct DriverContext;

impl<'a> DriverFactory<'a> {
  pub fn create_driver(&self, ctx: &'a DriverContext, execs: Vec<Box<Executor>>) -> Driver<'a>
  {
    Driver {
    	ctx: ctx,
    	execs: execs
    }
  }
}