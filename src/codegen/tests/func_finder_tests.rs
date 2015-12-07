#![feature(libc)]

extern crate libc;
extern crate llvm;
//extern crate llvm_sys;

#[macro_use]
extern crate util;

extern crate codegen;

use llvm::*;
use llvm::Attribute::*;
//use llvm_sys::core::LLVMSetLinkage;
//use llvm_sys::LLVMLinkage;

use std::mem;

use codegen::test_func3;

fn test_func_find(name: &str, lang: &str) {
  let ctx = Context::new();
  let module = Module::parse_bitcode(&ctx, "tests/test-module.bc").expect("loading test-module.bc failed...");
  
  //let ret_type = Type::get::<f64>(&ctx);
  //let param = vec![Type::get::<f64>(&ctx)];
  //let fn_ty = Type::function_ty(ret_type, &param);
  //let f = module.add_function("test_func3", fn_ty);
  //core::LLVMSetLinkage(f, LLVMLinkage::LLVMExternalLinkage);
  
  module.verify().expect("verifying the module failed...");
  
  for x in module.into_iter() {
  	x.add_attribute(Attribute::AlwaysInline);
  }
 
  let ee = JitEngine::new(&module, JitOptions {opt_level: 0}).unwrap();  
  let func = ee.find_function(name).expect(&format!("find_function: couldn't find {} from {}", name, lang));
	let f: fn(f64) -> f64;
  f = match unsafe { ee.get_function_raw(func) } {
  	Some(f) => unsafe { mem::transmute(f)},
  	_       => panic!("get_function_raw: couldn't find {} from {}", name, lang)
  };
  
  assert_eq!(98.0f64, f(98.0f64));
}

//#[test]
pub fn test_func_declaration() {
  let ctx = Context::new();
  let module = Module::parse_bitcode(&ctx, "tests/test-module.bc").expect("loading test-module.bc failed...");
  module.verify().expect("verifying the module failed...");
  
  for x in module.into_iter() {
  	x.add_attribute(Attribute::AlwaysInline);
  }
  
  let func = module.get_function("test_func1").unwrap();
  assert_eq!(1, func.num_params());
  
  let param = &func[0];
  assert_eq!(Type::get::<f64>(&ctx), param.get_type());
}

//#[test]
pub fn test_c_func() {
  test_func_find("test_func1", "c");
}

//#[test]
pub fn test_rust_func() {
  test_func_find("test_func2", "rust");
}

#[test]
pub fn test_rust_func2() {
  println!("{}", test_func3(1980.0f64));
  
  test_func_find("test_func3", "rust");
}