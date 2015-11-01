use std::rc::Rc;

use rand;

use common::err::{Result, Void, void_ok};
use common::func::{
  FuncKind,
  FuncSignature,
  InvokeAction,
  gen_no_arg_func
};
use common::plugin::TypeRegistry;
use common::rows::ROWBATCH_SIZE;
use common::rows::MiniPageWriter;

pub fn register_funcs(reg: &TypeRegistry, list: &mut Vec<(FuncSignature, InvokeAction)>) -> Void
{
  list.push(try!(gen_no_arg_func(reg, "rand", vec![], "i32", FuncKind::Scalar, Rc::new(rand_i32))));
  
  void_ok  
}

#[allow(unused_variables)]
pub fn rand_i32(builder: &mut MiniPageWriter, rownum: usize) 
{
  for pos in 0 .. rownum {
    builder.write_i32(rand::random::<i32>());
  }
}

 #[allow(unused_variables)]
pub fn rand_i64(builder: &mut MiniPageWriter, rownum: usize) 
{
  for pos in 0 .. rownum {
    builder.write_f32(rand::random::<f32>());
  }
}