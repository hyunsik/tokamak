use std::rc::Rc;

use rand;

use common::err::{Result, Void, void_ok};
use common::func::FuncKind;
use common::plugin::{PluginManager, TypeRegistry};
use common::plugin::util::register_noarg_fn;
use common::rows::ROWBATCH_SIZE;
use common::rows::MiniPageWriter;

pub fn register_funcs(pkg_mgr: &mut PluginManager) -> Void
{
	try!(register_noarg_fn(pkg_mgr, "rand", vec![], "i32", FuncKind::Scalar, Rc::new(rand_i32)));
	try!(register_noarg_fn(pkg_mgr, "rand_i64", vec![], "i64", FuncKind::Scalar, Rc::new(rand_i32)));
  
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