#![allow(dead_code)]

use std::mem;

use llvm_sys::core;
use llvm_sys::prelude::{
  LLVMBuilderRef,
  LLVMValueRef  
};
use libc::{c_char, c_uint};

use types::Ty;
use block::BasicBlock;
use value::{PhiNode, Value};

static NULL_NAME:[c_char; 1] = [0];

pub struct Builder(pub LLVMBuilderRef);
impl_dispose!(Builder, core::LLVMDisposeBuilder);

impl Builder {
  
  pub fn get_insert_block(&self) -> BasicBlock 
  {
    BasicBlock(unsafe { core::LLVMGetInsertBlock(self.0)})
  }
  
  /// Position the builder at `instr` within `block`.
  pub fn position_at(&self, block: &BasicBlock, instr: &Value) 
  {
    unsafe { core::LLVMPositionBuilder(self.0, block.0, instr.0) }
  }
  
  /// Position the builder at the end of `block`.
  pub fn position_at_end(&self, block: &BasicBlock) 
  {
    unsafe { core::LLVMPositionBuilderAtEnd(self.0, block.0) }
  }
  
  /// Build an instruction that returns from the function with void.
  pub fn create_ret_void(&self) -> Value 
  {
    Value(unsafe {core::LLVMBuildRetVoid(self.0)})
  }
  
  /// Build an instruction that returns from the function with `value`.
  pub fn create_ret(&self, value: &Value) -> Value 
  {
    Value(unsafe { core::LLVMBuildRet(self.0, value.0)})
  }
  
  /// Build an instruction that allocates an array with the element type `elem` and 
  /// the size `size`.
  ///
  /// The size of this array will be the size of `elem` times `size`.
  pub fn build_array_alloca(&self, elem: &Ty, size: &Value) -> Value 
  {
    Value(unsafe { 
    	core::LLVMBuildArrayAlloca(self.0, 
    														 elem.0, 
    														 size.0, 
    														 NULL_NAME.as_ptr() as *const c_char) 
    })
  }
  
  /// Build an instruction that allocates a pointer to fit the size of `ty` then returns this 
  /// pointer.
  ///
  /// Make sure to call `build_free` with the pointer value when you're done with it, or you're
  /// gonna have a bad time.
  pub fn create_alloca(&self, ty: &Ty) -> Value 
  {
    Value(unsafe { 
    	core::LLVMBuildAlloca(self.0, 
    												ty.0, 
    												NULL_NAME.as_ptr() as *const c_char) 
    })
  }
  
  /// Build an instruction that frees the `val`, which _MUST_ be a pointer that was returned
  /// from `build_alloca`.
  pub fn create_free(&self, val: &Value) -> Value 
  {
    Value(unsafe { core::LLVMBuildFree(self.0, val.0) })
  }
  
  /// Build an instruction that store the value `val` in the pointer `ptr`.
  pub fn create_store(&self, val: &Value, ptr: &Value) -> Value 
  {
    Value(unsafe { core::LLVMBuildStore(self.0, val.0, ptr.0) })
  }
  
  /// Build an instruction that branches to the block `dest`.
  pub fn create_br(&self, dest: &BasicBlock) -> Value 
  {
    Value(unsafe { core::LLVMBuildBr(self.0, dest.0)})
  }
  
  /// Build an instruction that branches to `if_block` if `cond` evaluates to true, and 
  /// `else_block` otherwise.
  pub fn create_cond_br(&self, cond: &Value, 
  	                    if_block: &BasicBlock, else_block: Option<&BasicBlock>) -> Value 
  {
    Value(unsafe { 
    	core::LLVMBuildCondBr(self.0, 
    		                    cond.0, 
    		                    if_block.0, 
    		                    mem::transmute(else_block)) 
    })
  }
  
  
  /// Build an instruction that yields to `true_val` if `cond` is equal to `1`, and `false_val` 
  /// otherwise.
  pub fn create_select(&self, cond: &Value, true_val: &Value, false_val: &Value) -> Value 
  {
    Value(unsafe { 
    	core::LLVMBuildSelect(self.0, 
    		                    cond.0, 
    		                    true_val.0, 
    		                    false_val.0, 
    		                    NULL_NAME.as_ptr()) })
  }
  
  /// Build an instruction that casts a value into a certain type.
  pub fn create_bit_cast(&self, value: &Value, dest: &Ty) -> Value 
  {
    Value(unsafe { core::LLVMBuildBitCast(
    		self.0, 
    		value.0, 
    		dest.0, 
    		NULL_NAME.as_ptr()) 
    })
  }
  
  /// Build an instruction that inserts a value into an aggregate data value.
  pub fn create_insert_value(&self, agg: &Value, elem: &Value, index: usize) -> Value 
  {
    Value(unsafe { 
    	core::LLVMBuildInsertValue(self.0, 
    		                         agg.0, 
    		                         elem.0, 
    		                         index as c_uint, 
    		                         NULL_NAME.as_ptr()) 
    })
  }
  
  /// Build an instruction that extracts a value from an aggregate type.
	pub fn create_extract_value(&self, agg: &Value, index: usize) -> Value {
    Value(unsafe { 
    	core::LLVMBuildExtractValue(self.0, 
    		                          agg.0, 
    		                          index as c_uint, 
    		                          NULL_NAME.as_ptr()) 
   	})
  }
  
  /// Build an instruction that computes the address of a subelement of an aggregate data 
  /// structure.
  ///
  /// Basically type-safe pointer arithmetic.
  pub fn create_gep(&self, pointer: &Value, indices: &[&Value]) -> Value 
  {
    let ref_array = to_llvmref_array!(indices, LLVMValueRef);
    
    Value(unsafe { 
    	core::LLVMBuildInBoundsGEP(self.0, 
    	                           pointer.0, 
    	                           ref_array.as_ptr() as *mut LLVMValueRef, 
    	                           indices.len() as c_uint, 
    	                           NULL_NAME.as_ptr()) 
    })
  }
  
  
  /// Build an instruction to select a value depending on the predecessor of the current block.
  pub fn create_phi(&self, ty: &Ty, name: &str) -> PhiNode 
  {
	  PhiNode(unsafe { 
	  	core::LLVMBuildPhi(self.0, ty.0, ::util::chars::from_str(name)) 
  	})
  } 
  
  /// Build an instruction that runs whichever block matches the value, or `default` if none of 
  /// them matched it.
  pub fn create_switch(&self, 
  	                   value: &Value, 
  	                   default: &BasicBlock, 
  	                   cases: &[(&Value, &BasicBlock)]) -> Value {
    Value(unsafe {
      let switch = core::LLVMBuildSwitch(self.0, 
      	                                 value.0, 
      	                                 default.0, 
      	                                 cases.len() as c_uint);
      for case in cases {
        core::LLVMAddCase(switch, (case.0).0, (case.1).0);
      }
      
      switch
    })
  }
}