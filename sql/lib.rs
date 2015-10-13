extern crate common;
extern crate rows;

pub mod types;

use common::err::{void_ok, Void};
use common::types::{Type};
use common::func::{FuncSignature, InvokeAction};
use common::plugin::{FuncRegistry, InputSourceRegistry, Package, TypeRegistry}; 

const PACKAGE_NAME: &'static str = "sql";

pub struct SQLPackage;

impl Package for SQLPackage {
  fn name(&self) -> &str { PACKAGE_NAME }
  
  fn load(&mut self, 
    type_reg: &mut TypeRegistry, 
    fn_reg: &mut FuncRegistry,
    src_reg: &mut InputSourceRegistry) -> Void {
      
      
    try!(type_reg.add_all(load_types()));
    try!(fn_reg.add_all(load_funcs()));
    void_ok()
  }
}

fn load_types() -> Vec<Box<Type>> {
  vec![
    Box::new(types::Int4::new())  
  ]
}

fn load_funcs() -> Vec<(FuncSignature, InvokeAction)> {
  vec![]
}