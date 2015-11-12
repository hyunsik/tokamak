#![feature(libc)]

extern crate libc;
extern crate llvm_sys;

#[macro_use]
extern crate util;

pub mod block;
pub mod common;
pub mod context;
pub mod func;
pub mod llvm;
pub mod module;
pub mod types;
pub mod value;

pub enum CodeGenErr
{
	LLVMInitializeNativeAsmParser,
	LLVMInitializeNativeAsmPrinter,
	LLVMInitializeNativeTargetError,
	NotFunctionTy,
}
