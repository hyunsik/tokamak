use llvm_sys::prelude::{
	LLVMTypeRef
};

use llvm_sys::core::{
	LLVMVoidTypeInContext
};

use context::Context;

macro_rules! ty {
	($e:expr) => ( Type::from_ref(unsafe { $e }))
}

pub struct Type {
	rf: LLVMTypeRef
}

/// Wrapper for LLVM TypeRef
impl Type {
	#[inline(always)]
  pub fn from_ref(r: LLVMTypeRef) -> Type 
  {
  	Type { rf: r }
  }
  
  pub fn void(ctx: &Context) -> Type {
		ty!(LLVMVoidTypeInContext(ctx.rf()))
  }
}