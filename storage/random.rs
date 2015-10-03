use std::rc::Rc;
use rand;

use common::err::{void_ok,Void,TResult};
use common::rows::{DefaultPageBuilder,Page,PageBuilder,RowId,ROWBATCH_SIZE,VectorBuilder};
use common::types::Type;

use super::InputSource;

pub struct RandomTableGenerator 
{
  types: Rc<Vec<Box<Type>>>,
  page_builder: Box<PageBuilder>,
  write_fns: Vec<Box<Fn(RowId, &VectorBuilder)>> 
}

fn write_rand_for_i32(builder: &mut VectorBuilder) 
{
  for pos in 0 .. ROWBATCH_SIZE {
    builder.write_i32(rand::random::<i32>());
  }
}

impl RandomTableGenerator 
{
  fn new(types: Rc<Vec<Box<Type>>>) -> RandomTableGenerator {
    
    RandomTableGenerator {
      types: types.clone(),
      page_builder: Box::new(DefaultPageBuilder::new(&*types)),
      write_fns: Vec::new()
    }
  }
}

impl InputSource for RandomTableGenerator 
{
  fn init(&mut self) -> Void 
  { 
    void_ok() 
  }
  
  fn has_next(&mut self) -> bool { true }
  
  fn next(&mut self) -> TResult<&Page> 
  {
    for ty in self.types.iter() {
      match ty.id().base() {
        "int4" => write_rand_for_i32(
        _ => {println!("xxx");}
      }            
    }
    
    Ok(self.page_builder.build())
  }
  
  fn close(&mut self) -> Void { void_ok() }
}