#![feature(libc)]

extern crate libc;
extern crate llvm;

#[macro_use]
extern crate util;

//use llvm::*;
//use llvm::Attribute::*;

#[no_mangle]
pub extern "C" fn test_func3(x: f64) -> f64 {
  x
}