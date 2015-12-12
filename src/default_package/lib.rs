extern crate rand;

#[macro_use]
extern crate common;

use common::err::{Void, void_ok};
use common::plugin::{Plugin, PluginManager};

mod arithm_funcs;
mod math_funcs;

const PACKAGE_NAME: &'static str = "default";

pub struct DefaultPackage;

impl Plugin for DefaultPackage
{
  fn name(&self) -> &str { PACKAGE_NAME }

  fn load(&self, mgr: &mut PluginManager) -> Void
  {
  	try!(arithm_funcs::register_funcs(PACKAGE_NAME, mgr));
  	try!(math_funcs::register_funcs(PACKAGE_NAME, mgr));

		void_ok
  }
}
