use std::marker::PhantomData;

use llvm_sys::prelude::{
	LLVMContextRef
};

use llvm_sys::core::{
	LLVMContextCreate,
	LLVMContextDispose,
};

pub struct Context<'c> {
	rf     : LLVMContextRef,
	marker : PhantomData<&'c ()>,
}

impl<'c> Drop for Context<'c> 
{
	fn drop(&mut self) 
	{ 
		unsafe { LLVMContextDispose(self.rf) } 
	}
}

impl<'c> Context<'c> {
  pub fn new() -> Context<'c> {
    let rf = unsafe { LLVMContextCreate() };
    Context { 
    	rf: rf, 
    	marker: PhantomData 
  	}
  }
  
  pub fn rf(&self) -> LLVMContextRef {
  	self.rf
  }
} 