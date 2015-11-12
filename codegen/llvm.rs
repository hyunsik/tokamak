use std::ffi::{CStr, CString};
use std::marker;
use std::mem;

use libc::{c_char};

use llvm_sys::prelude::{
	LLVMBasicBlockRef,
	LLVMContextRef,
	LLVMValueRef,
	LLVMModuleRef,
	LLVMTypeRef,
};
use llvm_sys::core::{
	LLVMContextCreate,
	LLVMContextDispose,
	LLVMDisposeModule,
	LLVMModuleCreateWithNameInContext,
	LLVMVoidTypeInContext
};
use llvm_sys::execution_engine::{
	LLVMLinkInMCJIT,
	LLVMCreateMCJITCompilerForModule,
	LLVMExecutionEngineRef,
	LLVMDisposeExecutionEngine,
	LLVMMCJITCompilerOptions,
	LLVMInitializeMCJITCompilerOptions
};
use llvm_sys::target;

use context::Context;
use module::Module;
use value::Value;
use super::CodeGenErr;

pub struct ExecutionEngine<'e>
{
	ee_ref: LLVMExecutionEngineRef,
	marker: marker::PhantomData<&'e ()>
}

impl<'e> Drop for ExecutionEngine<'e> {
  fn drop(&mut self) {
    unsafe {
      LLVMDisposeExecutionEngine(self.ee_ref);
    }
  }
}

impl<'e> ExecutionEngine<'e>
{
	pub fn new(m: Module) -> Result<ExecutionEngine<'e>, CodeGenErr>
	{
		let mut ee_ref: LLVMExecutionEngineRef;
		let mut error_msg: *mut c_char;
		
		unsafe {
			if 0 != target::LLVM_InitializeNativeAsmParser() {
        return Err(CodeGenErr::LLVMInitializeNativeAsmParser);
      }
			
			if 0 != target::LLVM_InitializeNativeAsmPrinter() {
				return Err(CodeGenErr::LLVMInitializeNativeAsmPrinter);
			}
			
			if 0 != target::LLVM_InitializeNativeTarget() {
	      return Err(CodeGenErr::LLVMInitializeNativeTargetError);
      }
			
			ee_ref = mem::uninitialized();
      error_msg = mem::uninitialized();
		}
		
		let mref = m.rf();
    mem::forget(m);
		
		let r = unsafe {
			let mut options: LLVMMCJITCompilerOptions = mem::uninitialized();
			let mut ptr: *mut LLVMMCJITCompilerOptions = &mut options;
			println!("Before LLVMInitializeMCJITCompilerOptions");
			LLVMInitializeMCJITCompilerOptions(ptr, mem::size_of::<LLVMMCJITCompilerOptions>() as u64);
			
			println!("After LLVMInitializeMCJITCompilerOptions");
			LLVMLinkInMCJIT();
			
			println!("LLVMLinkInMCJIT");
			
			LLVMCreateMCJITCompilerForModule(
				&mut ee_ref, 
				mref, 
				ptr, 
				mem::size_of::<LLVMMCJITCompilerOptions>() as u64, 
				&mut error_msg)   		
		};
		println!("LLVMCreateMCJITCompilerForModule");
		
		match r {
			  0 => Ok(ExecutionEngine { ee_ref: ee_ref, marker: marker::PhantomData }),
				_ => panic!("ExecutionEngine initialization failed: {}", 
							unsafe {CStr::from_ptr(error_msg).to_str().ok().unwrap()} )
 		}
	}
}

pub trait CodeGen 
{
	fn codegen<'c>(&self, ctx: &Context<'c>) -> Value<'c>;
}

#[test]
pub fn test_llvm() {
	let ctx    = Context::new();
	let module = Module::new(&ctx, "test");
	println!("MODULE: {}", module);
	let ee     = ExecutionEngine::new(module).ok().unwrap();
}