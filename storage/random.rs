use std::rc::Rc;
use rand;

use itertools::Zip;

use common::err::{void_ok,Void,TResult};
use common::types::Type;
use common::rows::{
  Page,
  PageBuilder,
  PosId,
  ROWBATCH_SIZE,
  MiniPageWriter
};


use super::InputSource;

pub struct RandomTableGenerator 
{
  types: Rc<Vec<Box<Type>>>,
  page_builder: Box<PageBuilder>,
  write_fns: Vec<Box<Fn(&mut MiniPageWriter)>> 
}

fn write_rand_for_i32(builder: &mut MiniPageWriter) 
{
  for pos in 0 .. ROWBATCH_SIZE {
    builder.write_i32(rand::random::<i32>());
  }
}

fn write_rand_for_f32(builder: &mut MiniPageWriter) 
{
  for pos in 0 .. ROWBATCH_SIZE {
    builder.write_f32(rand::random::<f32>());
  }
}

impl RandomTableGenerator 
{
  pub fn new(types: Rc<Vec<Box<Type>>>) -> RandomTableGenerator {
    
    RandomTableGenerator {
      types: types.clone(),
      page_builder: Box::new(PageBuilder::new(&*types)),
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
    for (ty, writer) in Zip::new((self.types.iter(), self.page_builder.iter_mut())) {
      match ty.id().base() {
        "int4" => write_rand_for_i32(writer),
        "float4" => write_rand_for_f32(writer),
        _ => {println!("xxx");}
      }
    }
    
    Ok(self.page_builder.build())
  }
  
  fn close(&mut self) -> Void { void_ok() }
}