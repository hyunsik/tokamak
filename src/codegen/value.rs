use std::fmt;

use libc::c_ulonglong;
use llvm_sys::core;
use llvm_sys::prelude::{
  LLVMValueRef,
  LLVMContextRef
};
use types::LLVMTy;

#[derive(Copy, Clone)]
pub struct Value(pub LLVMValueRef);
impl_display!(Value, LLVMPrintValueToString);

pub trait ToValue {
	/// Transform this value into a constant in the context given.
  fn to_value(self, ctx: LLVMContextRef) -> Value;
}


macro_rules! int_to_value (
  ($ty:ty) => (    
    impl ToValue for $ty {
      fn to_value(self, ctx: LLVMContextRef) -> Value 
      {
        Value(unsafe { core::LLVMConstInt(Self::get_ty(ctx).as_ptr(), self as c_ulonglong, 0) })
      }    
    }
  );
);


int_to_value!{i8}
int_to_value!{u8}  
int_to_value!{i16}
int_to_value!{u16}
int_to_value!{i32}
int_to_value!{u32}
int_to_value!{i64}
int_to_value!{u64}
int_to_value!{usize}
int_to_value!{isize}

impl ToValue for f32 {
  fn to_value(self, ctx: LLVMContextRef) -> Value
  {
    Value(unsafe{core::LLVMConstReal(Self::get_ty(ctx).as_ptr(), self as f64)})
  }
}

impl ToValue for f64 {
  fn to_value(self, ctx: LLVMContextRef) -> Value
  {
    Value(unsafe{core::LLVMConstReal(Self::get_ty(ctx).as_ptr(), self)})
  }
}

#[cfg(test)]
mod tests {
	use super::*;
  use super::super::*;
	
	#[test]
	pub fn test_values() 
	{
    let jit = JitCompiler::new("test").ok().unwrap();
    
    assert_eq!( "i8 1", format!("{}", 1i8.to_value(jit.context())));
    assert_eq!( "i8 1", format!("{}", 1u8.to_value(jit.context())));    
    assert_eq!("i16 1", format!("{}", 1i16.to_value(jit.context())));
    assert_eq!("i16 1", format!("{}", 1u16.to_value(jit.context())));
    assert_eq!("i32 1", format!("{}", 1i32.to_value(jit.context())));
    assert_eq!("i32 1", format!("{}", 1u32.to_value(jit.context())));
    assert_eq!("i64 1", format!("{}", 1i64.to_value(jit.context())));
    assert_eq!("i64 1", format!("{}", 1u64.to_value(jit.context())));
    assert_eq!("i64 1", format!("{}", 1isize.to_value(jit.context())));
    assert_eq!("i64 1", format!("{}", 1usize.to_value(jit.context())));
    
    assert_eq!("float 1.000000e+00", format!("{}", 1f32.to_value(jit.context())));
    assert_eq!("double 1.000000e+00", format!("{}", 1f64.to_value(jit.context())));
	}
}