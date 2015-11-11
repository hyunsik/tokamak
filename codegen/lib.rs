#![feature(libc)]

extern crate libc;
extern crate llvm_sys;

extern crate common;
#[macro_use]
extern crate util;

pub mod context;
pub mod llvm;
pub mod module;
pub mod types;
pub mod value;

pub enum CodeGenErr
{
	LLVMInitializeNativeAsmParser,
	LLVMInitializeNativeAsmPrinter,
	LLVMInitializeNativeTargetError
}