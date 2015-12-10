use std::fmt;

use llvm_sys::core;
use llvm_sys::prelude::{
  LLVMContextRef,
  LLVMTypeRef
};
use libc::c_uint;

pub struct Ty(pub LLVMTypeRef);
impl_display!(Ty, LLVMPrintTypeToString);

impl Ty {
  #[inline(always)]
  pub fn as_ptr(&self) -> LLVMTypeRef { self.0 }
}

pub trait LLVMTy {
  fn llvm_ty(ctx: LLVMContextRef) -> Ty;
}

macro_rules! impl_llvm_ty (
  ($ty:ty, $func:expr) => (
    impl LLVMTy for $ty {  
      fn llvm_ty(ctx: LLVMContextRef) -> Ty 
      {
        Ty(unsafe{$func(ctx)})
      }
    }   
  );
);

impl_llvm_ty!(i8,  core::LLVMInt8TypeInContext);
impl_llvm_ty!(u8,  core::LLVMInt8TypeInContext);
impl_llvm_ty!(i16, core::LLVMInt16TypeInContext);
impl_llvm_ty!(u16, core::LLVMInt16TypeInContext);
impl_llvm_ty!(i32, core::LLVMInt32TypeInContext);
impl_llvm_ty!(u32, core::LLVMInt32TypeInContext);
impl_llvm_ty!(i64, core::LLVMInt64TypeInContext);
impl_llvm_ty!(u64, core::LLVMInt64TypeInContext);
impl_llvm_ty!(f32, core::LLVMFloatTypeInContext);
impl_llvm_ty!(f64, core::LLVMDoubleTypeInContext);

impl LLVMTy for usize {
  fn llvm_ty(ctx: LLVMContextRef) -> Ty
  {
    Ty(unsafe{core::LLVMIntTypeInContext(ctx, ::std::mem::size_of::<isize>() as c_uint * 8)})
  }
}

impl LLVMTy for isize {
  fn llvm_ty(ctx: LLVMContextRef) -> Ty
  {
    Ty(unsafe{core::LLVMIntTypeInContext(ctx, ::std::mem::size_of::<isize>() as c_uint * 8)})
  }
}

#[cfg(test)]
mod tests {
	use super::*;
  use super::super::*;    
	
	#[test]
	pub fn test_types() 
	{
		let jit = JitCompiler::new("test").ok().unwrap();
    let ctx = jit.context();
		assert_eq!("i8",     format!("{}", i8::llvm_ty(ctx)));
		assert_eq!("i16",    format!("{}", i16::llvm_ty(ctx)));
		assert_eq!("i32",    format!("{}", i32::llvm_ty(ctx)));
		assert_eq!("i64",    format!("{}", i64::llvm_ty(ctx)));
		assert_eq!("float",  format!("{}", f32::llvm_ty(ctx)));
		assert_eq!("double", format!("{}", f64::llvm_ty(ctx)));
		
		//assert_eq!("[10 x double]",  format!("{}", Type::array_ty(&Type::f64_ty(&ctx), 10)));
	}
}