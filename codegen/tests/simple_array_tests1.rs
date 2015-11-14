#![feature(libc)]

extern crate libc;
extern crate llvm;

#[macro_use]
extern crate util;

use llvm::*;
use llvm::Attribute::*;

pub struct Test {
	id: i32
}

#[test]
pub fn test() {
  let ctx = Context::new();
  let module = Module::new("simple", &ctx);
  
  //let func = module.add_function("fib", Type::get::<fn(*const u64) -> u64>(&ctx));  
  //func.add_attributes(&[NoUnwind, ReadNone]);
  //let value = &func[0];
  
  //println!("{}", Type::get::<[u64;6]>(&ctx));
  //println!("{}", Type::get::<*const u64>(&ctx));
  //println!("{}", Type::get::<*const i32>(&ctx));
  //println!("{}", Type::get::<*const i8>(&ctx));
  //println!("{}", Type::get::<*const i16>(&ctx));
}