extern crate common;
extern crate rows;

pub mod types;

use std::rc::Rc;

use common::err::{void_ok, Void, Result};
use common::types::{Type, TypeFactory};
use common::func::{FuncSignature, InvokeAction};
use common::plugin::{FuncRegistry, InputSourceRegistry, Plugin, TypeRegistry}; 
use types::*;
const PACKAGE_NAME: &'static str = "sql";

pub struct SQLPackage;

impl Plugin for SQLPackage {
  fn name(&self) -> &str { PACKAGE_NAME }
  
  fn types(&self) -> Vec<(&'static str, TypeFactory)> {
    println!("Enter SQLPackage::types");
    let factory: Rc<Fn(&str) -> Result<Box<Type>>> = Rc::new(parse_type_str);
  
    vec![
        (INT4_STR  , factory.clone()),
        (FLOAT4_STR, factory.clone()),
    ]
  }
  
  fn funcs(&self) -> Vec<(FuncSignature, InvokeAction)> {
    vec![]
  }
}