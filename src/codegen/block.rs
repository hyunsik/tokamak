use llvm_sys::core;
use llvm_sys::prelude::LLVMBasicBlockRef;

use value::Value;

pub struct BasicBlock(pub LLVMBasicBlockRef);

impl BasicBlock 
{
  /// Return the enclosing method, or `None` if it is not attached to a method.
  pub fn get_parent(&self) -> Option<Value> 
  {
    unsafe { 
      let ptr = core::LLVMGetBasicBlockParent(self.0);
      
      if ptr.is_null() {
        None
      } else {
        Some(Value(ptr))
      }
    }
  }
  
  /// Move this basic block after the `other` basic block in its function.
  pub fn move_after(&self, other: &BasicBlock) 
  {
    unsafe { core::LLVMMoveBasicBlockAfter(self.0, other.0) }
  }
  
  /// Move this basic block before the `other` basic block in its function.
  pub fn move_before(&self, other: &BasicBlock) 
  {
    unsafe { core::LLVMMoveBasicBlockBefore(self.0, other.0) }
  }
  
  /// Unlink from the containing function, but do not delete it.
  pub fn remove(&self) 
  {
    unsafe { core::LLVMRemoveBasicBlockFromParent(self.0) }
  }
  
  /// Delete this basic block.
  ///
  /// This is unsafe because there should be no other reference to this, but
  /// this can't be guranteed using Rust semantics.
  pub unsafe fn delete(&self) 
  {
    core::LLVMDeleteBasicBlock(self.0)
  }
}