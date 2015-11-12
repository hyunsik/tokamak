use std::marker::PhantomData;

use llvm_sys::prelude::{
	LLVMBasicBlockRef,
	LLVMValueRef
};

use llvm_sys::core::LLVMBasicBlockAsValue;

use value::Value;

/// LLVMBlock
pub struct BasicBlock<'b> {
	rf     : LLVMBasicBlockRef,
	marker : PhantomData<&'b ()>,
}

impl<'b> BasicBlock<'b>
{
	pub fn to_ref(&self) -> LLVMBasicBlockRef {
    self.rf
  }

  pub fn as_value(self) -> Value<'b> {
    unsafe {
      Value::new(LLVMBasicBlockAsValue(self.to_ref()))
    }
  }
}