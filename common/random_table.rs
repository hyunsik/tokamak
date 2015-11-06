//! Random Data Generation Table
//!
//! It is designed for unit tests or users' prototypying.

use std::rc::Rc;
use rand;

use itertools::Zip;

use err::{void_ok, Void, Result};
use session::Session;
use types::{Ty};
use rows::{
  MiniPageWriter,
  Page,
  PageBuilder,
  ROWBATCH_SIZE
};
use input::InputSource;

pub struct RandomTableGenerator 
{
  builder: Box<PageBuilder>,
  write_fns: Vec<Box<Fn(&mut MiniPageWriter, usize)>>,
  rownum : usize 
}

impl RandomTableGenerator 
{
  pub fn new(session: &Session, types: &Vec<Ty>, rownum: usize) -> Box<InputSource> {
    
    Box::new(RandomTableGenerator {
      builder: Box::new(PageBuilder::new(types)),
      write_fns: types.iter()
        .map(|ty| choose_random_fn(ty)) // choose random functions for types
        .collect::<Vec<Box<Fn(&mut MiniPageWriter, usize)>>>(),
      rownum: rownum  
    })
  }
}

impl InputSource for RandomTableGenerator 
{
  fn open(&mut self) -> Void { void_ok }
  
  fn has_next(&mut self) -> bool { true }
  
  fn next(&mut self) -> Result<&Page> 
  {
    self.builder.reset();
    
    let min = ::std::cmp::min(self.rownum, ROWBATCH_SIZE);
    
    for (gen_fn, writer) in Zip::new((self.write_fns.iter(), self.builder.iter_mut())) {
      (gen_fn)(writer, min)
    }
    
    Ok(self.builder.build())
  }
  
  fn close(&mut self) -> Void { void_ok }
}

#[allow(unused_variables)]
fn write_rand_for_i32(builder: &mut MiniPageWriter, rownum: usize) 
{
  for pos in 0 .. rownum {
    builder.write_i32(rand::random::<i32>());
  }
}

 #[allow(unused_variables)]
fn write_rand_for_f32(builder: &mut MiniPageWriter, rownum: usize) 
{
  for pos in 0 .. rownum {
    builder.write_f32(rand::random::<f32>());
  }
}

fn choose_random_fn(ty: &Ty) -> Box<Fn(&mut MiniPageWriter, usize)> 
{
  match ty.base() {
    "i32" => Box::new(write_rand_for_i32),
    "f32" => Box::new(write_rand_for_f32),
    _ => panic!("not supported type")
  }
}