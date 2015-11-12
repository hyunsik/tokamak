use std::ffi::CString;
use std::fmt;
use std::marker::PhantomData;
use std::mem;

use llvm_sys::core::{
	LLVMDisposeModule,
	LLVMGetTarget, 
	LLVMModuleCreateWithNameInContext,
	LLVMPrintModuleToString,
	LLVMFunctionType
};
use llvm_sys::prelude::{
	LLVMModuleRef,
	LLVMTypeRef,
	LLVMValueRef
};

use common::False;
use context::Context;
use types::{Type, FunctionType};

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
		from_cstr!(LLVMGetTarget(self.rf))
	}
	
	pub fn rf(&self) -> LLVMModuleRef { self.rf }
	
	pub fn add_func(&self, 
								  ret_ty: Type, 
								  name: &str, 
								  arg_tys: Vec<Type>, 
								  var_arg: bool) -> FunctionType
	{
		
		let func_ty = unsafe { 
			FunctionType(Type::from_ref(LLVMFunctionType(ret_ty.to_ref(), 
											 mem::transmute(arg_tys.as_ptr()),
											 arg_tys.len() as u32,
											 0))) 
		};
		
		func_ty
	}
}

impl<'m> fmt::Display for Module<'m> {
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
  	write!(f, "{}", from_cstr!(LLVMPrintModuleToString(self.rf)))
  }
}