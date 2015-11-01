extern crate common;

pub mod types;

use std::rc::Rc;

use common::err::{void_ok, Void, Result};
use common::types::{Ty, TypeFactory};
use common::func::{FuncSignature, InvokeAction};
use common::plugin::{FuncRegistry, InputSourceRegistry, Plugin, PluginManager, TypeRegistry}; 
use types::*;
const PACKAGE_NAME: &'static str = "sql";

pub struct SQLPackage;

impl Plugin for SQLPackage {
  fn name(&self) -> &str { PACKAGE_NAME }
  
  fn load(&self, mgr: &mut PluginManager) -> Void
  {
  	void_ok
  } 
}