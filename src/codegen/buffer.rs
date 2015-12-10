use std::mem;

use llvm_sys::core;
use llvm_sys::prelude::LLVMMemoryBufferRef;
use libc::c_char;

use util::{chars_to_str, str_to_chars};

pub struct MemoryBuffer(LLVMMemoryBufferRef);

impl MemoryBuffer
{
  pub fn as_ptr(&self) -> LLVMMemoryBufferRef {
    self.0
  } 
  
  pub fn from_file(path: &str) -> Result<MemoryBuffer, String> 
  {
    let c_path = str_to_chars(path);
    
    unsafe {
      let mut out: LLVMMemoryBufferRef = mem::uninitialized();
      let mut err: *mut c_char = mem::uninitialized();
      
      let ret = core::LLVMCreateMemoryBufferWithContentsOfFile(c_path, &mut out, &mut err);      
      llvm_ret!(ret, MemoryBuffer(out), err)       
    }
  }
}

dispose_impl!(MemoryBuffer, core::LLVMDisposeMemoryBuffer);