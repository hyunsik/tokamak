use std::marker::PhantomData;

use llvm_sys::prelude::{
	LLVMValueRef
};

#[derive(Copy, Clone)]
pub struct Value<'c> 
{
  value_ref: LLVMValueRef,
  marker: PhantomData<&'c ()>,
}