#![allow(dead_code)]
use std::fmt;

use libc::c_ulonglong;
use llvm_sys::core;
use llvm_sys::prelude::{
  LLVMValueRef,
  LLVMContextRef
};

use types::LLVMTy;
use block::BasicBlock;

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
        Value(unsafe { core::LLVMConstInt(Self::llvm_ty(ctx).as_ptr(), self as c_ulonglong, 0) })
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
    Value(unsafe{core::LLVMConstReal(Self::llvm_ty(ctx).as_ptr(), self as f64)})
  }
}

impl ToValue for f64 {
  fn to_value(self, ctx: LLVMContextRef) -> Value
  {
    Value(unsafe{core::LLVMConstReal(Self::llvm_ty(ctx).as_ptr(), self)})
  }
}


/// A PHI node represents a value which is selected based on the predecessor of the current block.
pub struct PhiNode(pub LLVMValueRef);
impl_display!(PhiNode, LLVMPrintValueToString);

impl PhiNode {
  /// Adds an incoming value to the end of this PHI node.
  pub fn add_incoming(&self, val: &Value, block: &BasicBlock) {
    let mut values = vec![val.0];
    let mut blocks = vec![block.0];
    unsafe { core::LLVMAddIncoming(self.0, values.as_mut_ptr(), blocks.as_mut_ptr(), 1) }
  }
  
  /// Counts the number of incoming values attached to this PHI node.
  pub fn count_incoming(&self) -> u32 {
    unsafe { core::LLVMCountIncoming(self.0) }
  }
  
  /// Gets an incoming value from this PHI node from a specific index.
  pub fn get_incoming_value(&self, index: u32) -> Value {
    Value(unsafe { core::LLVMGetIncomingValue(self.0, index) })
  }
  
  /// Gets an incoming basic block from this PHI node from a specific index.
  pub fn get_incoming_block(&self, index: u32) -> BasicBlock {
    BasicBlock(unsafe { core::LLVMGetIncomingBlock(self.0, index)})
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