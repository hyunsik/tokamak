//! JIT Compiler to generate code fragments in runtime.

extern crate libc;
extern crate llvm_sys;

#[macro_use]
mod macros;
mod buffer;
mod util;
mod types;
mod value;

use std::mem;
use std::ptr;

use llvm_sys::core;
use llvm_sys::prelude::{
  LLVMContextRef,
  LLVMModuleRef,
  LLVMBuilderRef
};
use llvm_sys::bit_reader::LLVMParseBitcodeInContext;
use llvm_sys::execution_engine::{
  LLVMExecutionEngineRef,
  LLVMLinkInMCJIT,
  LLVMMCJITCompilerOptions,
  LLVMCreateMCJITCompilerForModule
};  
use llvm_sys::target::{
  LLVM_InitializeNativeTarget,
  LLVM_InitializeNativeAsmPrinter
};
use llvm_sys::target_machine::LLVMCodeModel;

use libc::{c_char, c_uint};

use buffer::MemoryBuffer;
use util::chars_to_str;

pub const JIT_OPT_LVEL: usize = 2;


fn new_module_from_bc(ctx: LLVMContextRef, path: &str) -> Result<LLVMModuleRef, String> 
{
  unsafe {
    let mut out: LLVMModuleRef = mem::uninitialized();
    let mut err: *mut c_char   = mem::uninitialized();
    let buf = try!(MemoryBuffer::from_file(path));
    
    let ret = LLVMParseBitcodeInContext(ctx, 
                                        buf.as_ptr(), 
    	                                  &mut out, 
    	                                  &mut err);
    llvm_ret!(ret, out, err)                                   
  }
}

fn new_jit_ee(m: LLVMModuleRef, opt_lv: usize) -> Result<LLVMExecutionEngineRef, String> 
{
  unsafe {
    let mut ee : LLVMExecutionEngineRef = mem::uninitialized();
    let mut err: *mut c_char = mem::uninitialized();
    
    LLVMLinkInMCJIT();
    expect_noerr!(LLVM_InitializeNativeTarget(), "failed to initialize native target");
    expect_noerr!(LLVM_InitializeNativeAsmPrinter(), "failed to initialize native asm printer");
    
    let mut opts = new_mcjit_compiler_options(opt_lv);
    let opts_size = mem::size_of::<LLVMMCJITCompilerOptions>();
    
    let ret = LLVMCreateMCJITCompilerForModule(&mut ee, 
      		                                     m, 
      		                                     &mut opts, 
      		                                     opts_size as u64, 
      		                                     &mut err);
    llvm_ret!(ret, ee, err)                                                       
  }
}

fn new_mcjit_compiler_options(opt_lv: usize) -> LLVMMCJITCompilerOptions
{
  LLVMMCJITCompilerOptions {
    OptLevel: opt_lv as c_uint,
    CodeModel: LLVMCodeModel::LLVMCodeModelJITDefault,
    NoFramePointerElim: 0,
    EnableFastISel: 1,
    MCJMM: ptr::null_mut()
  }
}

pub struct JitCompiler 
{
  ctx    : LLVMContextRef,
  module : LLVMModuleRef,
  ee     : LLVMExecutionEngineRef,
  builder: LLVMBuilderRef 
}

impl JitCompiler {
  pub fn new(bitcode_path: &str) -> Result<JitCompiler, String> 
  {
    let ctx     = unsafe { core::LLVMContextCreate() };
    let module  = try!(new_module_from_bc(ctx, bitcode_path));
    let ee      = try!(new_jit_ee(module, JIT_OPT_LVEL));
    let builder = unsafe { core::LLVMCreateBuilderInContext(ctx) };
    
    Ok(JitCompiler {
      ctx    : ctx,
      module : module,
      ee     : ee,
      builder: builder
    })
  }
  
  pub fn ctx(&self) -> LLVMContextRef { self.ctx }
  pub fn module(&self) -> LLVMModuleRef { self.module }
  pub fn engine(&self) -> LLVMExecutionEngineRef { self.ee }
  pub fn builder(&self) -> LLVMBuilderRef { self.builder }
}

impl Drop for JitCompiler {
  fn drop(&mut self) {
    unsafe {
      core::LLVMDisposeBuilder(self.builder);
      core::LLVMDisposeModule(self.module);      
      core::LLVMContextDispose(self.ctx);
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  
  #[test]
  fn test_ctor() {
    let jit = JitCompiler::new("target/test-ir/test-module.bc").ok().unwrap();
  }
}