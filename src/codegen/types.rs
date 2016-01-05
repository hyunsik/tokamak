#![allow(dead_code)]
use std::fmt;
use std::mem;

use llvm_sys::{core, LLVMTypeKind};
use llvm_sys::prelude::{
  LLVMContextRef,
  LLVMTypeRef
};
use libc::c_uint;

use super::LLVMRef;
use util::HasContext;

#[derive(Clone, Eq, PartialEq)]
pub struct Ty(pub LLVMTypeRef);
impl_from_ref!(LLVMTypeRef, Ty);
impl_display!(Ty, LLVMPrintTypeToString);
impl_has_context!(Ty, LLVMGetTypeContext);

impl Ty {
  #[inline(always)]
  pub fn as_ptr(&self) -> LLVMTypeRef { self.0 }
  
  #[inline(always)]
  pub fn void_ty<'a>(ctx: LLVMContextRef) -> Ty 
  {
  	Ty(unsafe { core::LLVMVoidTypeInContext(ctx) })
  }
  
  /// Returns true if the size of the type is known at compile-time.
  ///
  /// This is equivalent to the type implementing `Sized` in Rust
  #[inline(always)]
  pub fn is_sized(&self) -> bool 
  {
    unsafe { core::LLVMTypeIsSized(self.into()) != 0 }
  }
  
  /// Returns true if this type is a function.
  #[inline(always)]
  pub fn is_func_ty(&self) -> bool 
  {
    let kind = unsafe { core::LLVMGetTypeKind(self.into()) };
    kind as c_uint == LLVMTypeKind::LLVMFunctionTypeKind as c_uint
  }
  
  /// Returns true if this type is void.
  #[inline(always)]
  pub fn is_void(&self) -> bool 
  {
    let kind = unsafe { core::LLVMGetTypeKind(self.into()) };
    kind as c_uint == LLVMTypeKind::LLVMVoidTypeKind as c_uint
  }
  
  /// Returns true if this type is a pointer.
  #[inline(always)]
  pub fn is_pointer(&self) -> bool 
  {
    let kind = unsafe { core::LLVMGetTypeKind(self.into()) };
    kind as c_uint == LLVMTypeKind::LLVMPointerTypeKind as c_uint
  }
  
  /// Returns true if this type is an integer.
  #[inline(always)]
  pub fn is_integer(&self) -> bool 
  {
    let kind = unsafe { core::LLVMGetTypeKind(self.into()) };
    kind as c_uint == LLVMTypeKind::LLVMIntegerTypeKind as c_uint
  }
  
  /// Returns true if this type is any floating-point number.
  #[inline(always)]
  pub fn is_float(&self) -> bool 
  {
    let kind = unsafe { core::LLVMGetTypeKind(self.into()) } as c_uint;
    kind == LLVMTypeKind::LLVMHalfTypeKind as c_uint ||
    kind == LLVMTypeKind::LLVMFloatTypeKind as c_uint ||
    kind == LLVMTypeKind::LLVMDoubleTypeKind as c_uint
  }
}

/// A function signature type.
pub struct FunctionTy(pub LLVMTypeRef);
impl_display!(FunctionTy, LLVMPrintTypeToString);
impl_has_context!(FunctionTy, LLVMGetTypeContext);

impl FunctionTy {
  
  /// Returns the number of parameters this signature takes.
  pub fn num_params(&self) -> usize {
    unsafe { core::LLVMCountParamTypes(self.0) as usize }
  }

  /// Returns a vector of this signature's parameters' types.
  pub fn params(&self) -> Vec<Ty> {
  	unsafe {
    	let count = core::LLVMCountParamTypes(self.0);
      let mut types: Vec<LLVMTypeRef> = (0..count).map(|_| mem::uninitialized()).collect();
      core::LLVMGetParamTypes(self.0, types.as_mut_ptr() as *mut LLVMTypeRef);      
      types.into_iter().map(|t| Ty(t)).collect::<Vec<Ty>>()
    }
  }
  
  /// Returns the type that this function returns.
  pub fn ret_type(&self) -> Ty {
    Ty(unsafe { core::LLVMGetReturnType(self.0)})
  }
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
		let jit = JitCompiler::new("test1").ok().unwrap();
    let ctx = jit.context();
		assert_eq!("i8",     format!("{}", i8::llvm_ty(ctx)));
		assert_eq!("i16",    format!("{}", i16::llvm_ty(ctx)));
		assert_eq!("i32",    format!("{}", i32::llvm_ty(ctx)));
		assert_eq!("i64",    format!("{}", i64::llvm_ty(ctx)));
		assert_eq!("float",  format!("{}", f32::llvm_ty(ctx)));
		assert_eq!("double", format!("{}", f64::llvm_ty(ctx)));
		
		//assert_eq!("[10 x double]",  format!("{}", Type::array_ty(&Type::f64_ty(&ctx), 10)));
	}  
  
  #[test]
  fn test_function_ty() {
    let jit = JitCompiler::new("test2").ok().unwrap();
    let ctx = jit.context();    
    let prototype = 
      jit.create_func_ty(&f64::llvm_ty(ctx), &[&i8::llvm_ty(ctx), &i16::llvm_ty(ctx)]);
    
    assert_eq!(prototype.ret_type(), f64::llvm_ty(ctx));
    
    assert_eq!(prototype.num_params(), 2);
    assert_eq!(prototype.params().len(), 2);
    assert_eq!(prototype.params().get(0).unwrap(), &i8::llvm_ty(ctx));
    assert_eq!(prototype.params().get(1).unwrap(), &i16::llvm_ty(ctx));
  }
}