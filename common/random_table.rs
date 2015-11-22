//! Random Data Generation Table
//!
//! It is designed for unit tests or users' prototypying.

use std::rc::Rc;
use rand;

use itertools::Zip;

use err::{Error, Result, void_ok, Void};
use session::Session;
use types::{Ty};
use rows::{
  MiniPageWriter,
  Page,
  OwnedPageBuilder,
  ROWBATCH_SIZE
};
use input::InputSource;

pub struct RandomTable 
{  
  builder  : Box<OwnedPageBuilder>,
  write_fns: Vec<Box<Fn(&mut MiniPageWriter, usize)>>,
  row_num  : usize, // number of rows to generate   
  cur_pos  : usize  // how many rows are generated so far?
}

impl RandomTable 
{
  pub fn new(session: &Session, types: &Vec<Ty>, row_num: usize) -> Box<InputSource> {
    
    Box::new(RandomTable {      
      builder: Box::new(OwnedPageBuilder::new(types)),
      write_fns: types.iter()
        .map(|ty| choose_random_fn(ty)) // choose random functions for types
        .collect::<Vec<Box<Fn(&mut MiniPageWriter, usize)>>>(),
      row_num: row_num,
      cur_pos: 0  
    })
  }
}

impl InputSource for RandomTable 
{
  fn open(&mut self) -> Void { void_ok }
  
  fn has_next(&mut self) -> bool { true }
  
  fn next(&mut self) -> Result<&Page> 
  {
    self.builder.reset();
    
    if self.cur_pos >= self.row_num {
    	return Ok(self.builder.build(0))
    }
    
    // determine the row number to generate at this call
    let remain = self.row_num - self.cur_pos;  
    let min = ::std::cmp::min(remain, ROWBATCH_SIZE);
    
    for (gen_fn, writer) in Zip::new((self.write_fns.iter(), self.builder.iter_mut())) {
      (gen_fn)(writer, min)
    }
    // move forward the position
    self.cur_pos += min;
    
    Ok(self.builder.build(min))
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