#![feature(libc)]

extern crate libc;
extern crate llvm;

extern crate codegen;

use llvm::*;
use llvm::Attribute::*;
use std::mem;

fn test_func_find(module: &Module, name: &str, lang: &str) 
{
  let ee = JitEngine::new(module, JitOptions {opt_level: 0}).unwrap();  
  let func = ee.find_function(name).expect(&format!("find_function: couldn't find {} from {}", name, lang));
	let f: fn(f64) -> f64;
  f = match unsafe { ee.get_function_raw(func) } {
  	Some(f) => unsafe { mem::transmute(f)},
  	_       => panic!("get_function_raw: couldn't find {} from {}", name, lang)
  };
  
  assert_eq!(98.0f64, f(98.0f64));
}

#[test]
pub fn test_c_func() 
{
  let ctx = Context::new();
  let module = Module::parse_bitcode(&ctx, "tests/test-module.bc").expect("loading test-module.bc failed...");
  module.verify().expect("verifying the module failed...");
  
  let func = module.get_function("test_func1").unwrap();
  assert_eq!(1, func.num_params());  
  let param = &func[0];
  assert_eq!(Type::get::<f64>(&ctx), param.get_type());
  
  test_func_find(&module, "test_func1", "c");
  test_func_find(&module, "test_func2", "rust");
}