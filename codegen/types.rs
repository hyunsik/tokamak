use std::fmt;

use llvm_sys::LLVMTypeKind;

use llvm_sys::prelude::{
	LLVMTypeRef
};

use llvm_sys::core::{
	LLVMArrayType,
	LLVMFloatTypeInContext,
	LLVMDoubleTypeInContext,
	LLVMGetTypeKind,
	LLVMInt1TypeInContext,
	LLVMInt8TypeInContext,
	LLVMInt16TypeInContext,
	LLVMInt32TypeInContext,
	LLVMInt64TypeInContext,
	LLVMPrintTypeToString,
	LLVMVoidTypeInContext,
};

use context::Context;

macro_rules! ty {
	($e:expr) => ( Type::from_ref(unsafe { $e }))
}

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
  
  pub fn kind(&self) -> LLVMTypeKind 
  {
    unsafe { 
      LLVMGetTypeKind(self.rf)
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
}

impl fmt::Display for Type {
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
  	write!(f, "{}", from_cstr!(LLVMPrintTypeToString(self.rf)))
  }
}

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
	}
}