//! JIT Compiler to generate code fragments in runtime.

extern crate libc;
extern crate llvm_sys;

#[macro_use]
mod macros;
mod block;
mod buffer;
mod builder;
mod util;
mod types;
mod value;

use std::mem;
use std::ptr;

use llvm_sys::analysis;
use llvm_sys::core;
use llvm_sys::prelude::{
  LLVMContextRef,
  LLVMModuleRef
};
use llvm_sys::bit_reader::LLVMParseBitcodeInContext;
use llvm_sys::execution_engine::{
  LLVMExecutionEngineRef,
  LLVMLinkInMCJIT,
  LLVMMCJITCompilerOptions,
  LLVMCreateMCJITCompilerForModule
};
use llvm_sys::linker;
use llvm_sys::transforms::pass_manager_builder as pass;  
use llvm_sys::target::{
  LLVM_InitializeNativeTarget,
  LLVM_InitializeNativeAsmPrinter
};
use llvm_sys::target_machine::LLVMCodeModel;

use libc::{c_char, c_uint};

use buffer::MemoryBuffer;
use builder::Builder;
use value::{GlobalValue, Value, ValueIter, ValueRef};
use types::Ty;
use util::chars;

pub const JIT_OPT_LVEL: usize = 2;


#[repr(C)]
#[derive(Copy, Clone)]
pub enum AddressSpace 
{
  Generic = 0,
  Global = 1,
  Shared = 3,
  Const = 4,
  Local = 5,
}


pub trait LLVMRef<T> {
  fn as_ref(&self) -> T;
}


fn new_module(ctx: LLVMContextRef, name: &str) -> LLVMModuleRef
{
  let c_name = util::chars::from_str(name);
  unsafe { 
  	core::LLVMModuleCreateWithNameInContext(c_name, ctx) 
  }
}
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
  //builder: Builder // internal use
}

impl JitCompiler {
  
  pub fn new(module_name: &str) -> Result<JitCompiler, String>
  {
    let ctx     = unsafe { core::LLVMContextCreate() };
    let module  = new_module(ctx, module_name);
    JitCompiler::new_internal(ctx, module)
  }
  
  pub fn new_from_bc(bitcode_path: &str) -> Result<JitCompiler, String> 
  {
    let ctx     = unsafe { core::LLVMContextCreate() };
    let module  = try!(new_module_from_bc(ctx, bitcode_path));
    JitCompiler::new_internal(ctx, module)
  }
  
  fn new_internal(ctx: LLVMContextRef, module: LLVMModuleRef) -> Result<JitCompiler, String>
  {
    let ee      = try!(new_jit_ee(module, JIT_OPT_LVEL));
    //let builder = Builder(unsafe { core::LLVMCreateBuilderInContext(ctx) });
    
    Ok(JitCompiler {
      ctx    : ctx,
      module : module,
      ee     : ee,
      //builder: builder
    })
  }
  
  pub fn context(&self) -> LLVMContextRef { self.ctx }
  pub fn module(&self) -> LLVMModuleRef { self.module }
  pub fn engine(&self) -> LLVMExecutionEngineRef { self.ee }
  
  /// Returns the target data of the base module represented as a string
  pub fn get_target(&self) -> &str 
  {
    unsafe {
      let target = core::LLVMGetTarget(self.module);
      chars::to_str(target)
    }
  }
  
  /// Get the data layout of the base module
  pub fn get_data_layout(&self) -> &str 
  {
  	unsafe {
  		let layout = core::LLVMGetDataLayout(self.module);
  		chars::to_str(layout as *mut c_char)
  	}
  } 
  
  /// Link a module into this module, returning an error string if an error occurs.
  ///
  /// This *does not* destroy the source module.
  pub fn link(&self, module: LLVMModuleRef) -> Result<(), String> 
  {
    unsafe {
      let mut error = mem::uninitialized();      
      let ret = linker::LLVMLinkModules(self.module, module, 
      	                                linker::LLVMLinkerMode::LLVMLinkerPreserveSource, 
      	                                &mut error);
      llvm_ret!(ret, (), error)
    }
  }
  
  /// Link a module into this module, returning an error string if an error occurs.
  ///
  /// This *does* destroy the source module.
  pub fn link_destroy(&self, module: LLVMModuleRef) -> Result<(), String> 
  {
    unsafe {
      let mut error = mem::uninitialized();      
      let ret = linker::LLVMLinkModules(self.module, module, 
      	                                linker::LLVMLinkerMode::LLVMLinkerDestroySource, 
      	                                &mut error);
      llvm_ret!(ret, (), error)
    }
  }
  
  /// Optimize this module with the given optimization level and size level.
  ///
  /// This runs passes depending on the levels given.
  pub fn optimize(&self, opt_level: usize, size_level: usize) {
    unsafe {
      let builder = pass::LLVMPassManagerBuilderCreate();
      pass::LLVMPassManagerBuilderSetOptLevel(builder, opt_level as c_uint);
      pass::LLVMPassManagerBuilderSetSizeLevel(builder, size_level as c_uint);
      let pass_manager = core::LLVMCreatePassManager();
      pass::LLVMPassManagerBuilderPopulateModulePassManager(builder, pass_manager);
      pass::LLVMPassManagerBuilderDispose(builder);
      core::LLVMRunPassManager(pass_manager, self.module);
    }
  }
  
  /// Verify that the module is safe to run, returning a string detailing the error
  /// when an error occurs.
  pub fn verify(&self) -> Result<(), String> 
  {
    unsafe {
      let mut error = mem::uninitialized();
      let action = analysis::LLVMVerifierFailureAction::LLVMReturnStatusAction;
      let ret = analysis::LLVMVerifyModule(self.module, action, &mut error);
      
      llvm_ret!(ret, (), error)
    }
  }
  
  /// Dump the module to stderr (for debugging).
  pub fn dump(&self) 
  {
  	unsafe { core::LLVMDumpModule(self.module); }
  }  
  
  pub fn new_builder(&self) -> Builder 
  { 
    Builder(unsafe { core::LLVMCreateBuilderInContext(self.ctx) }) 
  }
  
  /// Returns the type with the name given, or `None`` if no type with that name exists.
  pub fn get_type<'a>(&'a self, name: &str) -> Option<Ty> 
  {
    let c_name = chars::from_str(name);
    unsafe {
      let ty = core::LLVMGetTypeByName(self.module, c_name);
      ::util::ret_nullable_ptr(ty)
    }
  }
  
  /// Add an external global to the module with the given type and name.
  pub fn add_global(&self, name: &str, ty: &Ty) -> GlobalValue 
  {    
    let c_name = chars::from_str(name);        
    GlobalValue(
      unsafe {
        core::LLVMAddGlobal(self.module, ty.0, c_name)
      }
    )
  }
  
  /// Add a global in the given address space to the module with the given type and name.
  pub fn add_global_in_addr_space(&self, 
  	                              name: &str, 
  	                              ty: &Ty, 
  	                              sp: AddressSpace) -> GlobalValue 
  {
    let c_name = chars::from_str(name);        
    GlobalValue(
      unsafe {
        core::LLVMAddGlobalInAddressSpace(self.module, ty.0, c_name, sp as c_uint)
      }
    )
  }
  
  /// Add a constant global to the module with the given type, name and value.
  pub fn add_global_constant(&self, name: &str, val: &Value) -> GlobalValue 
  {
    let c_name = chars::from_str(name);
    GlobalValue(
      unsafe {    
        let global = core::LLVMAddGlobal(self.module, val.ty().0, c_name);
        core::LLVMSetInitializer (global, val.0);
        global
      }
    )
  }
  
  /// Get the global with the name given, or `None` if no global with that name exists.
  pub fn get_global(&self, name: &str) -> Option<GlobalValue> 
  {
    let c_name = chars::from_str(name);
    unsafe {
      let ptr = core::LLVMGetNamedGlobal(self.module, c_name);
      ::util::ret_nullable_ptr(ptr)
    }
  }
  
  /// Get an iterator of global values
  pub fn global_values(&self) -> ValueIter<GlobalValue>
  {
  	ValueIter::new(
 			unsafe { core::LLVMGetFirstGlobal(self.module) },
 			core::LLVMGetNextGlobal
 		) 
  }    
}

impl Drop for JitCompiler {
  fn drop(&mut self) {
    unsafe {
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