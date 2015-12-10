use libc::c_ulonglong;
use llvm_sys::core;
use llvm_sys::prelude::{
  LLVMValueRef,
  LLVMContextRef
};
use types::LLVMTy;

#[derive(Copy, Clone)]
pub struct Value(pub LLVMValueRef);

/// A type that can be represented as a constant in LLVM IR.
pub trait ToValue {
	/// Compile this value into a constant in the context given.
  fn to_value(self, ctx: LLVMContextRef) -> Value;
}


macro_rules! int_to_value (
  ($ty:ty) => (    
    impl ToValue for $ty {
      fn to_value(self, ctx: LLVMContextRef) -> Value 
      {
        Value(unsafe { core::LLVMConstInt(Self::get_ty(ctx).as_ptr(), self as c_ulonglong, 0) })
      }    
    }
  );
);


int_to_value!{i8}
int_to_value!{u8}  
int_to_value!{i16}
int_to_value!{u16}
int_to_value!{i32}
int_to_value!{u32}
int_to_value!{i64}
int_to_value!{u64}
int_to_value!{usize}
int_to_value!{isize}

impl ToValue for f32 {
  fn to_value(self, ctx: LLVMContextRef) -> Value
  {
    Value(unsafe{core::LLVMConstReal(Self::get_ty(ctx).as_ptr(), self as f64)})
  }
}

impl ToValue for f64 {
  fn to_value(self, ctx: LLVMContextRef) -> Value
  {
    Value(unsafe{core::LLVMConstReal(Self::get_ty(ctx).as_ptr(), self)})
  }
}