extern crate common;
extern crate rows;

pub mod types;

use common::types::Type;
use common::func::{FuncSignature, InvokeAction};
use common::plugin::Package;

pub struct SQLPackage {
  types: Vec<Box<Type>>,
  funcs: Vec<(FuncSignature, InvokeAction)>
}

impl SQLPackage {
  pub fn new() -> SQLPackage {
    SQLPackage {
      types: load_types(),
      funcs: Vec::new()
    }
  }
}

fn load_types() -> Vec<Box<Type>> {
  vec![
    Box::new(types::Int4::new())  
  ]
}