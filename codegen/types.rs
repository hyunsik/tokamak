use std::fmt;
use std::mem;
use std::ptr;

use llvm_sys::LLVMTypeKind;

use llvm_sys::prelude::{
	LLVMTypeRef
};

use llvm_sys::core::{
	LLVMArrayType,
	LLVMCountParamTypes,
	LLVMFloatTypeInContext,
	LLVMFunctionType,
	LLVMDoubleTypeInContext,
	LLVMGetParamTypes,
	LLVMGetReturnType,
	LLVMGetTypeKind,
	LLVMInt1TypeInContext,
	LLVMInt8TypeInContext,
	LLVMInt16TypeInContext,
	LLVMInt32TypeInContext,
	LLVMInt64TypeInContext,
	LLVMPointerType,
	LLVMPrintTypeToString,
	LLVMVoidTypeInContext,
};

use context::Context;
use super::CodeGenErr;

macro_rules! ty {
	($e:expr) => ( Type::from_ref(unsafe { $e }))
}

#[derive(Clone)]
pub struct Type 
{
	rf: LLVMTypeRef
}

/// Wrapper for LLVM TypeRef
impl Type {
	#[inline(always)]
  pub fn from_ref(r: LLVMTypeRef) -> Type 
  {
  	Type { rf: r }
  }
  
  pub fn to_ref(&self) -> LLVMTypeRef
  {
  	self.rf
  }
  
  pub fn kind(&self) -> LLVMTypeKind 
  {
    unsafe { 
      LLVMGetTypeKind(self.rf)
    }
  }
  
  #[inline]
  pub fn is_function_ty(&self) -> bool
  {
  	match self.kind() {
  		LLVMTypeKind::LLVMFunctionTypeKind => true,
  		_                                  => false
		}
  }
  
  pub fn void(ctx: &Context) -> Type 
  {
		ty!(LLVMVoidTypeInContext(ctx.rf()))
  }
  
  pub fn bool(ccx: &Context) -> Type 
  {
		Type::i8(ccx)
  }
  
  pub fn i8(ccx: &Context) -> Type 
  {
    ty!(LLVMInt8TypeInContext(ccx.rf()))
  }
  
  pub fn i16(ccx: &Context) -> Type 
  {
    ty!(LLVMInt16TypeInContext(ccx.rf()))
  }
  
  pub fn i32(ccx: &Context) -> Type 
  {
    ty!(LLVMInt32TypeInContext(ccx.rf()))
  }

  pub fn i64(ccx: &Context) -> Type 
  {
    ty!(LLVMInt64TypeInContext(ccx.rf()))
  }
  
  pub fn f32(ccx: &Context) -> Type 
  {
    ty!(LLVMFloatTypeInContext(ccx.rf()))
  }
	
  pub fn f64(ccx: &Context) -> Type 
  {
    ty!(LLVMDoubleTypeInContext(ccx.rf()))
  }
  
  pub fn array(ty: &Type, size: u32) -> Type
  {
  	ty!(LLVMArrayType(ty.rf, size))
	}
  
  pub fn func(args: &[Type], ret: &Type) -> Type
  {
		ty!(LLVMFunctionType(ret.to_ref(), 
												 mem::transmute(args.as_ptr()),
                         args.len() as u32, ::common::False))
  }
  
  pub fn ptr_ty(&self) -> Type 
  {
		ty!(LLVMPointerType(self.rf, 0))
  }
  
  pub fn ret_ty(&self) -> Result<Type, CodeGenErr> 
  {
  	if self.is_function_ty() {
  		Ok(ty!(LLVMGetReturnType(self.rf)))
 		} else {
  		Err(CodeGenErr::NotFunctionTy)
  	}		
  }
  
  pub fn func_params(&self) -> Result<Vec<Type>, CodeGenErr> 
  {
  	if self.is_function_ty() {
	    unsafe {
	      let n_args = LLVMCountParamTypes(self.rf) as usize;
	      let mut args = vec![Type {rf: ptr::null_mut()}; n_args];
	      LLVMGetParamTypes(self.rf, args.as_mut_ptr() as *mut LLVMTypeRef);
	      
	      Ok(args)
	    }
    } else {
    	Err(CodeGenErr::NotFunctionTy)
    }
  }
}

impl fmt::Display for Type {
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
  	write!(f, "{}", from_cstr!(LLVMPrintTypeToString(self.rf)))
  }
}

#[macro_export]
macro_rules! subtype {
	($sub_ty:ident, $super_ty:ident) => {
  	impl<'c> ::std::ops::Deref for $sub_ty {
    	type Target = $super_ty;
      fn deref(&self) -> &Self::Target { &self.0 }
    }

    pub struct $sub_ty (pub $super_ty);
  }
}

subtype!(FunctionType, Type);
subtype!(PointerType , Type);

#[cfg(test)]
mod tests {
	use context::Context;
	use super::*;
	
	#[test]
	pub fn test_types() 
	{
		let ctx    = Context::new();
		assert_eq!("i8",     format!("{}", Type::i8(&ctx)));
		assert_eq!("i16",    format!("{}", Type::i16(&ctx)));
		assert_eq!("i32",    format!("{}", Type::i32(&ctx)));
		assert_eq!("i64",    format!("{}", Type::i64(&ctx)));
		assert_eq!("float",  format!("{}", Type::f32(&ctx)));
		assert_eq!("double", format!("{}", Type::f64(&ctx)));
		assert_eq!("[10 x double]",  format!("{}", Type::array(&Type::f64(&ctx), 10)));
		assert_eq!("[10 x double]*", format!("{}", Type::array(&Type::f64(&ctx), 10).ptr_ty()));
		
		assert!(!Type::i8(&ctx).is_function_ty());
		assert!(!Type::i16(&ctx).is_function_ty());
		assert!(!Type::i32(&ctx).is_function_ty());
		assert!(!Type::i64(&ctx).is_function_ty());
		assert!(!Type::f32(&ctx).is_function_ty());
		assert!(!Type::f64(&ctx).is_function_ty());
		assert!(!Type::array(&Type::f64(&ctx), 10).is_function_ty());
		assert!(!Type::array(&Type::f64(&ctx), 10).ptr_ty().is_function_ty());
	}
}