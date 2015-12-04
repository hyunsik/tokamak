#![feature(libc)]

extern crate libc;
extern crate llvm;

#[macro_use]
extern crate util;

use llvm::*;
use llvm::Attribute::*;

use std::mem;

#[test]
pub fn test() {
  let ctx = Context::new();
  let module = Module::parse_ir(&ctx, "tests/test-ir.ll").expect("loading test-ir.ll failed...");
  module.verify().expect("verifying the module failed...");
  
  for x in module.into_iter() {
  	x.add_attribute(Attribute::AlwaysInline);
  }
 
  let ee = JitEngine::new(&module, JitOptions {opt_level: 0}).unwrap();
  let func = ee.find_function("test_func1").expect("No such a function: test_func1");

	let f: fn(f64) -> f64;
  f = match unsafe { ee.get_function_raw(func) } {
  	Some(f) => unsafe { mem::transmute(f)},
  	_       => panic!("get_function_raw:: no such function" )
  };
  
  assert_eq!(98.0f64, f(98.0f64));
}