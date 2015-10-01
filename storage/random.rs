use std::rc::Rc;

use common::types::Type;
use super::InputSource;
use common::err::{Void, TResult, void_ok};
use common::rows::{Page, PageBuilder, DefaultPageBuilder};

pub struct RandomTableGenerator 
{
  types: Rc<Vec<Box<Type>>>,
  page_builder: Box<PageBuilder>
}

impl RandomTableGenerator 
{
  fn new(types: Rc<Vec<Box<Type>>>) -> RandomTableGenerator {
    
    RandomTableGenerator {
      types: types.clone(),
      page_builder: Box::new(DefaultPageBuilder::new(&*types))
    }
  }
}

impl InputSource for RandomTableGenerator 
{
  fn init(&mut self) -> Void { 
    void_ok() 
  }
  
  fn has_next(&mut self) -> bool { true }
  
  fn next(&mut self) -> TResult<&Page> {
    Ok(self.page_builder.build())
  }
  
  fn close(&mut self) -> Void { void_ok() }
}