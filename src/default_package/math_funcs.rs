use std::rc::Rc;

use rand;

use common::err::{Void, void_ok};
use common::plugin::{PluginManager};
use common::plugin::util::*;
use common::rows::MiniPageWriter;

pub fn register_funcs(namespace: &str, pkg_mgr: &mut PluginManager) -> Void
{
	register_noarg_fn!(pkg_mgr, namespace, "rand",     "i32", rand_i32);
	register_noarg_fn!(pkg_mgr, namespace, "rand_i64", "i64", rand_i64);

  void_ok
}

#[allow(unused_variables)]
pub fn rand_i32(builder: &mut MiniPageWriter, rownum: usize) -> Void
{
  for pos in 0 .. rownum {
    builder.write_i32(rand::random::<i32>());
  }

  void_ok
}

 #[allow(unused_variables)]
pub fn rand_i64(builder: &mut MiniPageWriter, rownum: usize) -> Void
{
  for pos in 0 .. rownum {
    builder.write_f32(rand::random::<f32>());
  }

  void_ok
}
