#![feature(libc)]

extern crate libc;
extern crate llvm_sys;

extern crate common;
extern crate util;

pub mod context;
pub mod llvm;
pub mod module;
pub mod ty;
pub mod value;

pub enum CodeGenErr
{
	LLVMInitializeNativeAsmParser,
	LLVMInitializeNativeAsmPrinter,
	LLVMInitializeNativeTargetError
}