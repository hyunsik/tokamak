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
  let module = Module::parse_bitcode(&ctx, "tests/test-module.bc").expect("loading test-module.bc failed...");
  module.verify().expect("verifying the module failed...");
  
  for x in module.into_iter() {
  	x.add_attribute(Attribute::AlwaysInline);
  }
  
  module.dump();
 
  let ee = JitEngine::new(&module, JitOptions {opt_level: 0}).unwrap();
  
  let func1 = ee.find_function("test_func1").expect("No such a function: test_func1");
	let f1: fn(f64) -> f64;
  f1 = match unsafe { ee.get_function_raw(func1) } {
  	Some(f) => unsafe { mem::transmute(f)},
  	_       => panic!("get_function_raw:: no such function" )
  };
  
  assert_eq!(98.0f64, f1(98.0f64));
  
  let func2 = ee.find_function("test_func2").expect("No such a function: test_func2");
	let f2: fn(f64) -> f64;
  f2 = match unsafe { ee.get_function_raw(func2) } {
  	Some(f) => unsafe { mem::transmute(f)},
  	_       => panic!("get_function_raw:: no such function" )
  };
    
  assert_eq!(98.0f64, f2(98.0f64));
}