#![allow(dead_code)]
use std::fmt;
use std::mem;

use libc::{c_char, c_int, c_uint, c_ulonglong};
use llvm_sys::core;
use llvm_sys::LLVMAttribute;
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


pub struct Arg(pub LLVMValueRef);
impl_from_ref!(LLVMValueRef, Arg);
impl_display!(Arg, LLVMPrintValueToString);

impl Arg 
{
  /// Add the attribute given to this argument.
  pub fn add_attribute(&self, attr: Attribute) 
  {
    unsafe { core::LLVMAddAttribute(self.0, attr.into()) }
  }
  
  /// Add all the attributes given to this argument.
  pub fn add_attributes(&self, attrs: &[Attribute]) 
  {
    let mut sum = LLVMAttribute::empty();
    for attr in attrs {
      let attr:LLVMAttribute = (*attr).into();
      sum = sum | attr;
    }
    unsafe { core::LLVMAddAttribute(self.into(), sum.into()) }
  }
  
  /// Returns true if this argument has the attribute given.
  pub fn has_attribute(&self, attr: Attribute) -> bool 
  {
    unsafe {
      let other = core::LLVMGetAttribute(self.into());
      other.contains(attr.into())
    }
  }
  
  /// Returns true if this argument has all the attributes given.
  pub fn has_attributes(&self, attrs: &[Attribute]) -> bool 
  {
    unsafe {
      let other = core::LLVMGetAttribute(self.into());
      for &attr in attrs {
        if !other.contains(attr.into()) {
          return false;
        }
      }
      return true;
    }
  }
  
  /// Remove an attribute from this argument.
  pub fn remove_attribute(&self, attr: Attribute) 
  {
    unsafe { 
    	core::LLVMRemoveAttribute(self.into(), attr.into())
    }
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
  
  /// Returns the entry block of this function or `None` if there is none.
  pub fn get_entry(&self) -> Option<BasicBlock> 
  {
    Some(
      BasicBlock(
        unsafe { mem::transmute(core::LLVMGetEntryBasicBlock(self.0)) }
      )
    )
  }
  
  /// Returns the type of this value
  pub fn signature(&self) -> FunctionType 
  {
    FunctionType(unsafe { core::LLVMTypeOf(self.0)})
  }
  
  /// Returns the number of function parameters
  pub fn args_count(&self) -> usize
  {
    unsafe {
      core::LLVMCountParams(self.into()) as usize
    }
  }
  
  pub fn arg(&self, index: usize) -> Arg
  {
    unsafe {
      if index < core::LLVMCountParams(self.into()) as usize {
        Arg(core::LLVMGetParam(self.into(), index as c_uint))
      } else {
        panic!("Argument index out of range {} at {:?}", index, self.signature())
      }
    }
  }
  
  /// Add the attribute given to this function.
  pub fn add_attribute(&self, attr: Attribute) 
  {
    unsafe { core::LLVMAddFunctionAttr(self.into(), attr.into()) }
  }
  
  /// Add all the attributes given to this function.
  pub fn add_attributes(&self, attrs: &[Attribute]) 
  {
    let mut sum = LLVMAttribute::empty();
    
    for attr in attrs {
        let attr:LLVMAttribute = (*attr).into();
        sum = sum | attr;
    }
    
    unsafe { core::LLVMAddFunctionAttr(self.into(), sum.into()) }
  }
  
  /// Returns true if the attribute given is set in this function.
  pub fn has_attribute(&self, attr: Attribute) -> bool 
  {
    unsafe {
      let other = core::LLVMGetFunctionAttr(self.into());
      other.contains(attr.into())
    }
  }
  
  /// Returns true if all the attributes given is set in this function.
  pub fn has_attributes(&self, attrs: &[Attribute]) -> bool 
  {
    unsafe {
      let other = core::LLVMGetFunctionAttr(self.into());
      
      for &attr in attrs {
        if !other.contains(attr.into()) {
          return false;
        }
      }
      
      return true;
    }
  }
  
  /// Remove the attribute given from this function.
  pub fn remove_attribute(&self, attr: Attribute) 
  {
    unsafe { core::LLVMRemoveFunctionAttr(self.into(), attr.into()) }
  }
}

impl IntoIterator for Function 
{
  type Item = Arg;
  type IntoIter = ValueIter<Arg>;
  
  /// Iterate through the functions in the module
  fn into_iter(self) -> ValueIter<Arg> 
  {    
 		ValueIter::new(
 			unsafe { core::LLVMGetFirstParam(self.into()) },
 			core::LLVMGetNextParam)  	
  }
}

/// A way of indicating to LLVM how you want arguments / functions to be handled.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
#[repr(C)]
pub enum Attribute 
{
  /// Zero-extended before or after call.
  ZExt =              0b1,
  /// Sign-extended before or after call.
  SExt =              0b10,
  /// Mark the function as not returning.
  NoReturn =          0b100,
  /// Force argument to be passed in register.
  InReg =             0b1000,
  /// Hidden pointer to structure to return.
  StructRet =         0b10000,
  /// Function doesn't unwind stack.
  NoUnwind =          0b100000,
  /// Consider to not alias after call.
  NoAlias =           0b1000000,
  /// Pass structure by value.
  ByVal =             0b10000000,
  /// Nested function static chain.
  Nest =              0b100000000,
  /// Function doesn't access memory.
  ReadNone =          0b1000000000,
  /// Function only reads from memory.
  ReadOnly =          0b10000000000,
  /// Never inline this function.
  NoInline =          0b100000000000,
  /// Always inline this function.
  AlwaysInline =      0b1000000000000,
  /// Optimize this function for size.
  OptimizeForSize =   0b10000000000000,
  /// Stack protection.
  StackProtect =      0b100000000000000,
  /// Stack protection required.
  StackProtectReq =   0b1000000000000000,
  /// Alignment of parameter (5 bits) stored as log2 of alignment with +1 bias 0 means unaligned (different from align(1)).
  Alignment =         0b10000000000000000,
  /// Function creates no aliases of pointer.
  NoCapture =         0b100000000000000000,
  /// Disable redzone.
  NoRedZone =         0b1000000000000000000,
  /// Disable implicit float instructions.
  NoImplicitFloat =   0b10000000000000000000,
  /// Naked function.
  Naked =             0b100000000000000000000,
  /// The source language has marked this function as inline.
  InlineHint =        0b1000000000000000000000,
  /// Alignment of stack for function (3 bits) stored as log2 of alignment with +1 bias 0 means unaligned (different from alignstack=(1)).
  StackAlignment =    0b11100000000000000000000000000,
  /// This function returns twice.
  ReturnsTwice =      0b100000000000000000000000000000,
  /// Function must be in unwind table.
  UWTable =           0b1000000000000000000000000000000,
  /// Function is called early/often, so lazy binding isn't effective.
  NonLazyBind =       0b10000000000000000000000000000000
}

impl From<LLVMAttribute> for Attribute 
{
  fn from(attr: LLVMAttribute) -> Attribute 
  {
    unsafe { mem::transmute(attr) }
  }
}

impl From<Attribute> for LLVMAttribute 
{
  fn from(attr: Attribute) -> LLVMAttribute 
  {
    unsafe { mem::transmute(attr) }
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
pub struct ValueIter<T: From<LLVMValueRef>> {
  cur    : LLVMValueRef,
  step   : unsafe extern "C" fn(LLVMValueRef) -> LLVMValueRef,
  marker : ::std::marker::PhantomData<T>,
}

impl<T: From<LLVMValueRef>> ValueIter<T>
{
  pub fn new(cur: LLVMValueRef, 
             step: unsafe extern "C" fn(LLVMValueRef) -> LLVMValueRef) -> Self
	{
    ValueIter {
      cur   : cur,
      step  : step,
      marker: ::std::marker::PhantomData
    }
  }
}

impl<T: From<LLVMValueRef>> Iterator for ValueIter<T> 
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

