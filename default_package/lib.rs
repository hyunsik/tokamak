extern crate rand;

extern crate common;

use std::mem;
use std::rc::Rc;

use common::err::{Error, Result, Void, void_ok};
use common::types::{Ty, TypeFactory, TypeHandler};
use common::func::{FuncSignature, InvokeAction};
use common::plugin::{Plugin, PluginManager};
use common::rows::{FMiniPage, MiniPage}; 

mod math_func;

const PACKAGE_NAME: &'static str = "default";

pub struct DefaultPackage;

impl Plugin for DefaultPackage 
{
  fn name(&self) -> &str { PACKAGE_NAME }
  
  fn load(&self, mgr: &mut PluginManager) -> Void 
  {
  	let factory: Rc<Fn(&str) -> Result<Ty>> = Rc::new(parse_type_str);
  	try!(mgr.register_ty((I32_STR, factory.clone())));
  	try!(mgr.register_ty((F32_STR, factory.clone())));
  	
  	math_func::register_funcs(mgr); 
  	
		void_ok  	
  }
}

pub fn parse_type_str(type_str: &str) -> Result<Ty> {
  match type_str {
  	I32_STR    => {
  		let f = || -> Box<MiniPage> {Box::new(FMiniPage::new(mem::size_of::<i32>()))};
  		Ok(Ty::new(I32_STR, true, true, TypeHandler {create_minipage: Rc::new(f)}))
    },
  	F32_STR    => {
  		let f = || -> Box<MiniPage> {Box::new(FMiniPage::new(mem::size_of::<f32>()))};
  		Ok(Ty::new(F32_STR, true, true, TypeHandler {create_minipage: Rc::new(f)}))
    },
    _          => Err(Error::UndefinedDataType(type_str.to_string()))
  }
}

pub const I32_STR        : &'static str = "i32";
pub const F32_STR        : &'static str = "f32";
pub const STR_STR        : &'static str = "str";