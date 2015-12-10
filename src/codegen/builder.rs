use llvm_sys::core;
use llvm_sys::prelude::{
  LLVMBuilderRef
};

pub struct Builder(pub LLVMBuilderRef);

impl_dispose!(Builder, core::LLVMDisposeBuilder);