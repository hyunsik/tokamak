use std::marker::PhantomData;

use llvm_sys::prelude::{
	LLVMValueRef
};

#[derive(Copy, Clone)]
pub struct Value<'c> 
{
  rf: LLVMValueRef,
  marker: PhantomData<&'c ()>,
}

impl<'v> Value<'v>
{
	pub fn new(rf: LLVMValueRef) -> Value<'v>
	{
		Value {
			rf    : rf,
			marker: PhantomData
		}
	}
}