extern crate common;

pub mod types;

use std::rc::Rc;

use common::err::{void_ok, Void, Result};
use common::types::{Ty, TypeFactory};
use common::func::{FuncSignature, InvokeAction};
use common::plugin::{FuncRegistry, InputSourceRegistry, Plugin, TypeRegistry}; 
use types::*;
const PACKAGE_NAME: &'static str = "sql";

pub struct SQLPackage;

impl Plugin for SQLPackage {
  fn name(&self) -> &str { PACKAGE_NAME }
  
  fn types(&self) -> Vec<(&'static str, TypeFactory)> {
    println!("Enter SQLPackage::types");
    let factory: Rc<Fn(&str) -> Result<Box<Ty>>> = Rc::new(parse_type_str);
  
    vec![
    ]
  }
  
  fn funcs(&self) -> Vec<(FuncSignature, InvokeAction)> {
    vec![]
  }
}