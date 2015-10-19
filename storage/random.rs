use std::rc::Rc;
use rand;

use itertools::Zip;

use common::err::{void_ok, Void, Result};
use common::types::Type;
use common::rows::{
  MiniPageWriter,
  Page,
  PageBuilder,
  ROWBATCH_SIZE
};
use common::input::InputSource;

pub struct RandomTableGenerator 
{
  builder: Box<PageBuilder>,
  write_fns: Vec<Box<Fn(&mut MiniPageWriter)>> 
}

impl RandomTableGenerator 
{
  pub fn new(types: Rc<Vec<Box<Type>>>) -> RandomTableGenerator {
    
    RandomTableGenerator {
      builder: Box::new(PageBuilder::new(&*types)),
      write_fns: types.iter()
        .map(|ty| choose_random_fn(&**ty)) // choose random functions for types
        .collect::<Vec<Box<Fn(&mut MiniPageWriter)>>>()
    }
  }
}

impl InputSource for RandomTableGenerator 
{
  fn open(&mut self) -> Void { void_ok }
  
  fn has_next(&mut self) -> bool { true }
  
  fn next(&mut self) -> Result<&Page> 
  {
    self.builder.reset();
    
    for (gen_fn, writer) in Zip::new((self.write_fns.iter(), self.builder.iter_mut())) {
      (gen_fn)(writer)
    }
    
    Ok(self.builder.build())
  }
  
  fn close(&mut self) -> Void { void_ok }
}

#[allow(unused_variables)]
fn write_rand_for_i32(builder: &mut MiniPageWriter) 
{
  for pos in 0 .. ROWBATCH_SIZE {
    builder.write_i32(rand::random::<i32>());
  }
}

 #[allow(unused_variables)]
fn write_rand_for_f32(builder: &mut MiniPageWriter) 
{
  for pos in 0 .. ROWBATCH_SIZE {
    builder.write_f32(rand::random::<f32>());
  }
}

fn choose_random_fn(ty: &Type) -> Box<Fn(&mut MiniPageWriter)> 
{
  match ty.id().base() {
    "int4"   => Box::new(write_rand_for_i32),
    "float4" => Box::new(write_rand_for_f32),
    _ => panic!("not supported type")
  }
}