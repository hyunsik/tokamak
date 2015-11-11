use std::ffi::CString;
use std::fmt;
use std::marker::PhantomData;

use llvm_sys::core::{
	LLVMDisposeModule,
	LLVMGetTarget, 
	LLVMModuleCreateWithNameInContext,
	LLVMPrintModuleToString
};
use llvm_sys::prelude::LLVMModuleRef;

use util::ffi::chars_to_str;
use context::Context;

pub struct Module<'m> {    
	rf: LLVMModuleRef,
	marker: PhantomData<&'m ()>,
}

impl<'m> Drop for Module<'m> {
	fn drop(&mut self) {
  	unsafe { LLVMDisposeModule(self.rf); }
  }
}

impl<'m> Module<'m> {
	pub fn new<'c:'m>(ctx: &Context<'c>, name: &str) -> Module<'m> {
  	let name = CString::new(name).unwrap();
    let mod_rf = unsafe {
        LLVMModuleCreateWithNameInContext(name.as_ptr(), ctx.rf())
    };
    
    Module { rf: mod_rf, marker: PhantomData }
  }
	
	pub fn get_target(&self) -> &str
	{
		unsafe {chars_to_str(LLVMGetTarget(self.rf))}
	}
	
	pub fn rf(&self) -> LLVMModuleRef { self.rf }
}

impl<'m> fmt::Display for Module<'m> {
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
  	write!(f, "{}", unsafe {chars_to_str(LLVMPrintModuleToString(self.rf))})
  }
}