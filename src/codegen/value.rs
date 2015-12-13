#![allow(dead_code)]
use std::fmt;

use libc::{c_char, c_int, c_uint, c_ulonglong};
use llvm_sys::core;
use llvm_sys::prelude::{
  LLVMValueRef,
  LLVMContextRef
};

use super::LLVMRef;
use types::{FunctionType, LLVMTy, Ty};
use block::BasicBlock;
use util::HasContext;

/// Common functions for LLVMValueRef
///
/// Refer to http://llvm.org/docs/doxygen/html/group__LLVMCCoreValueGeneral.html
pub trait ValueRef: LLVMRef<LLVMValueRef> {
  /// Returns the name of this value, or `None` if it lacks a name
  #[inline]
  fn name<'a>(&self) -> Option<&'a str> 
  {
    unsafe {
      let c_name = core::LLVMGetValueName(self.as_ref());
      ::util::chars::to_nullable_str(c_name)
    }
  }
  
  /// Sets the name of this value
  #[inline]
  fn set_name(&self, name: &str) 
  {
    let c_name = ::util::chars::from_str(name);
    unsafe {
      core::LLVMSetValueName(self.as_ref(), c_name);
    }
  }
  
  /// Returns the type of this value
  #[inline]
  fn ty(&self) -> Ty 
  {
    Ty(unsafe { core::LLVMTypeOf(self.as_ref())})
  }
  
  #[inline]
  fn dump(&self) 
  {
  	unsafe { core::LLVMDumpValue(self.as_ref()); }
  }
}

#[derive(Copy, Clone)]
pub struct Value(pub LLVMValueRef);
impl_from_ref!(LLVMValueRef, Value);
impl_display!(Value, LLVMPrintValueToString);
impl ValueRef for Value {}

impl Value {
  
  /// Create a new constant struct from the values given.
  pub fn new_struct(ctx: LLVMContextRef, 
  	                vals: &[&Value], 
                    packed: bool) -> Value 
  {                      
    let ref_array = to_llvmref_array!(vals, LLVMValueRef);
    
    Value(
      unsafe { 
    	  core::LLVMConstStructInContext(ctx, 
    	                                 ref_array.as_ptr() as *mut LLVMValueRef, 
    		                               vals.len() as c_uint, 
       	                               packed as c_int)
      }
    )
  }
  
  /// Create a new constant vector from the values given.
  pub fn new_vector<'a>(vals: &[&'a Value]) -> Value 
  {
    let ref_array = to_llvmref_array!(vals, LLVMValueRef);
    
    Value(
      unsafe { 
    	  core::LLVMConstVector(ref_array.as_ptr() as *mut LLVMValueRef, 
    	                        vals.len() as c_uint)
      }
    )
  }
  
  /// Create a new constant C string from the text given.
  pub fn new_string<'a>(ctx: LLVMContextRef, 
  	                    text: &str, 
  	                    rust_style: bool) -> Value 
  {
    Value(
      unsafe {
        let ptr = text.as_ptr() as *const c_char;
        let len = text.len() as c_uint;
        core::LLVMConstStringInContext(ctx, ptr, len, rust_style as c_int)
      }
    )
  }
  
  /// Create a new constant undefined value of the given type.
  pub fn new_undef<'a>(ty: &'a Ty) -> Value
  {
    Value(unsafe { core::LLVMGetUndef(ty.0) })
  }
}

pub trait ToValue {
	/// Transform this value into a constant in the context given.
  fn to_value(self, ctx: LLVMContextRef) -> Value;
}


macro_rules! int_to_value (
  ($ty:ty) => (    
    impl ToValue for $ty {
      fn to_value(self, ctx: LLVMContextRef) -> Value 
      {
        Value(unsafe { core::LLVMConstInt(Self::llvm_ty(ctx).as_ptr(), self as c_ulonglong, 0) })
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
    Value(unsafe{core::LLVMConstReal(Self::llvm_ty(ctx).as_ptr(), self as f64)})
  }
}

impl ToValue for f64 {
  fn to_value(self, ctx: LLVMContextRef) -> Value
  {
    Value(unsafe{core::LLVMConstReal(Self::llvm_ty(ctx).as_ptr(), self)})
  }
}


pub struct GlobalValue(pub LLVMValueRef);
impl_from_ref!(LLVMValueRef, GlobalValue);
impl_display!(GlobalValue, LLVMPrintValueToString);
impl ValueRef for GlobalValue {}

impl GlobalValue 
{
	/// Sets the initial value for this global.
	pub fn set_initializer(&self, value: &Value) 
	{
	  unsafe { core::LLVMSetInitializer(self.0, value.0) }
	}
	
	/// Gets the initial value for this global.
	pub fn get_initializer(&self) -> Value 
	{
	  Value(unsafe { core::LLVMGetInitializer(self.0) })
	}
}


/// A function that can be called and contains blocks.
pub struct Function(pub LLVMValueRef);
impl_from_ref!(LLVMValueRef, Function);
impl_display!(Function, LLVMPrintValueToString);
impl ValueRef for Function {}

impl HasContext for Function 
{
  fn context(&self) -> LLVMContextRef 
  {
    self.ty().context()
  }
}


impl Function {
  /// Add a basic block with the name given to the function and return it.
  pub fn append<'a>(&'a self, name: &str) -> BasicBlock 
  {
    let c_name = ::util::chars::from_str(name);
    BasicBlock(unsafe {
      core::LLVMAppendBasicBlockInContext(self.context(), self.0, c_name)
    })
  }
  
  /// Returns the type of this value
  pub fn ty(&self) -> FunctionType 
  {
    FunctionType(unsafe { core::LLVMTypeOf(self.0)})
  }
}


/// A PHI node represents a value which is selected based on the predecessor of the current block.
pub struct PhiNode(pub LLVMValueRef);
impl_from_ref!(LLVMValueRef, PhiNode);
impl_display!(PhiNode, LLVMPrintValueToString);
impl ValueRef for PhiNode {}

impl PhiNode {
  /// Adds an incoming value to the end of this PHI node.
  pub fn add_incoming(&self, val: &Value, block: &BasicBlock) {
    let mut values = vec![val.0];
    let mut blocks = vec![block.0];
    unsafe { core::LLVMAddIncoming(self.0, values.as_mut_ptr(), blocks.as_mut_ptr(), 1) }
  }
  
  /// Counts the number of incoming values attached to this PHI node.
  pub fn count_incoming(&self) -> u32 {
    unsafe { core::LLVMCountIncoming(self.0) }
  }
  
  /// Gets an incoming value from this PHI node from a specific index.
  pub fn get_incoming_value(&self, index: u32) -> Value {
    Value(unsafe { core::LLVMGetIncomingValue(self.0, index) })
  }
  
  /// Gets an incoming basic block from this PHI node from a specific index.
  pub fn get_incoming_block(&self, index: u32) -> BasicBlock {
    BasicBlock(unsafe { core::LLVMGetIncomingBlock(self.0, index)})
  }
}

/// Value Iterator implementation.
///
/// T can be all descendent types of LLVMValueRef.  
#[derive(Copy, Clone)]
pub struct ValueIter<'a, T: From<LLVMValueRef>> {
  cur    : LLVMValueRef,
  step   : unsafe extern "C" fn(LLVMValueRef) -> LLVMValueRef,
  marker1: ::std::marker::PhantomData<&'a ()>,
  marker2: ::std::marker::PhantomData<T>,
}

impl<'a, T: From<LLVMValueRef>> ValueIter<'a, T>
{
	pub fn new(cur: LLVMValueRef, 
		         step: unsafe extern "C" fn(LLVMValueRef) -> LLVMValueRef) -> Self
	{
		ValueIter {
			cur: cur,
			step: step,
			marker1: ::std::marker::PhantomData,
			marker2: ::std::marker::PhantomData
		}
	}
}

impl<'a, T: From<LLVMValueRef>> Iterator for ValueIter<'a, T> 
{
  type Item = T;

  fn next(&mut self) -> Option<T> 
  {
    let old: LLVMValueRef = self.cur;
    
    if !old.is_null() {
      self.cur = unsafe { (self.step)(old) };
      Some(old.into())
    } else {
      None
    }
  }
}

#[cfg(test)]
mod tests {
	use super::*;
  use super::super::*;
  
  use llvm_sys::prelude::LLVMValueRef;
  
	#[test]
	pub fn test_values() 
	{
    let jit = JitCompiler::new("test").ok().unwrap();
    
    assert_eq!( "i8 1", format!("{}", 1i8.to_value(jit.context())));
    assert_eq!( "i8 1", format!("{}", 1u8.to_value(jit.context())));    
    assert_eq!("i16 1", format!("{}", 1i16.to_value(jit.context())));
    assert_eq!("i16 1", format!("{}", 1u16.to_value(jit.context())));
    assert_eq!("i32 1", format!("{}", 1i32.to_value(jit.context())));
    assert_eq!("i32 1", format!("{}", 1u32.to_value(jit.context())));
    assert_eq!("i64 1", format!("{}", 1i64.to_value(jit.context())));
    assert_eq!("i64 1", format!("{}", 1u64.to_value(jit.context())));
    assert_eq!("i64 1", format!("{}", 1isize.to_value(jit.context())));
    assert_eq!("i64 1", format!("{}", 1usize.to_value(jit.context())));
    
    assert_eq!("float 1.000000e+00", format!("{}", 1f32.to_value(jit.context())));
    assert_eq!("double 1.000000e+00", format!("{}", 1f64.to_value(jit.context())));
	}
  
  pub fn test_into() {
    let jit = JitCompiler::new("test").ok().unwrap();
        
    let v1 = 1i8.to_value(jit.context());    
    let ref_v1 = &v1;
    
    let raw_ref1: LLVMValueRef = v1.into();
    let raw_ref2: LLVMValueRef = ref_v1.into();    
    
    assert_eq!(v1.as_ref(), raw_ref1);
    assert_eq!(v1.as_ref(), raw_ref2);
    assert_eq!(raw_ref1, raw_ref2);    
  }
}

