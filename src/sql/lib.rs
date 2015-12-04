extern crate common;

use common::err::{void_ok, Void};
use common::plugin::{Plugin, PluginManager}; 

const PACKAGE_NAME: &'static str = "sql";

pub struct SQLPackage;

impl Plugin for SQLPackage {
  fn name(&self) -> &str { PACKAGE_NAME }
  
  #[allow(unused_variables)]
  fn load(&self, mgr: &mut PluginManager) -> Void
  {
  	void_ok
  } 
}