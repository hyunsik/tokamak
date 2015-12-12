use std::ops;
use std::rc::Rc;

use common::err::{Void, void_ok};
use common::plugin::PluginManager;
use common::plugin::util::*;
use common::rows::{MiniPage, MiniPageWriter};

pub fn register_funcs(namespace: &str, pkg_mgr: &mut PluginManager) -> Void
{
	register_unary_fn!(pkg_mgr, namespace, "+", "i32", vec!["i32", "i32"], plus_v_v::<i32>);

  void_ok
}

#[allow(unused_variables)]
pub fn plus_v_v<T: ops::Add>(
		writer: &mut MiniPageWriter,
		input: &MiniPage,
		selected: Option<u32>,
		row_num: usize) -> Void
{

	if selected.is_some() {
		let selvec = selected.unwrap();
	} else {

	}

  void_ok
}
