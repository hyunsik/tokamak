#![feature(libc)]

extern crate libc;
extern crate llvm;

#[macro_use]
extern crate util;

use llvm::*;
use llvm::Attribute::*;

use std::mem;

#[no_mangle]
pub extern fn cos1(a: f64) -> f64 {
	1.2345f64
}

#[test]
pub fn test() {
  let ctx = Context::new();
  let module = Module::parse_ir(&ctx, "../llvm-ir/llvm-ir.ll").ok().unwrap();
  module.verify().unwrap();
  
  for x in module.into_iter() {
  	println!(">>>> Function Name: {}", x.get_name());
  	x.add_attribute(Attribute::AlwaysInline);
  }
  
  module.dump();
 
  let ee = JitEngine::new(&module, JitOptions {opt_level: 0}).unwrap();
  let func = ee.find_function("cos1").unwrap();

	let cos: fn(f64) -> f64;
  cos = match unsafe { ee.get_function_raw(func) } {
  	Some(f) => unsafe { mem::transmute(f)},
  	_       => panic!("SS")
  };
  println!(">>>> 3");
  println!("{}", cos(98.0f64));
}