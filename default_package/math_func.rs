use std::rc::Rc;

use rand;

use common::err::{Result, Void, void_ok};
use common::plugin::{PluginManager, TypeRegistry};
use common::plugin::util::*;
use common::rows::ROWBATCH_SIZE;
use common::rows::MiniPageWriter;

pub fn register_funcs(pkg_mgr: &mut PluginManager) -> Void
{
	register_noarg_fn!(pkg_mgr, "rand",     vec![], "i32", rand_i32);
	register_noarg_fn!(pkg_mgr, "rand_i64", vec![], "i64", rand_i64);
  
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