//! JIT Compiler to generate code fragments in runtime.

#![feature(libc)]
extern crate libc;
extern crate llvm;

extern crate common;

use std::borrow::Borrow;
use std::marker::PhantomData;

use llvm::*;

use common::err::{Void, void_ok};

pub const JIT_OPT_LVEL: usize = 2;

pub struct JitEngineBuilder<'a>
{
  ctx: &'a Context
}

impl<'a> JitEngineBuilder<'a>
{
  pub fn new(ctx: &'a Context) -> JitEngineBuilder<'a>
  {
    JitEngineBuilder {
      ctx: ctx      
    }
  }
  
  pub fn new_module(&'a self, name: &str) -> ModuleBuilder<'a> 
  {
    ModuleBuilder {
      ctx: self.ctx,
      module: Module::new(name, self.ctx)
    }
  }
  
  pub fn from_bitcode(&'a mut self, path: &str) -> ModuleBuilder<'a> 
  {
    ModuleBuilder {
      ctx: self.ctx,
      module: Module::parse_bitcode(self.ctx, path)
                .expect(&format!("loading bitcode at '{}' failed...", path))
    }
  }
  
  pub fn from_ir(&'a mut self, path: &str) -> ModuleBuilder<'a> 
  {
    ModuleBuilder {
      ctx: self.ctx,
      module: Module::parse_ir(self.ctx, path)
                .expect(&format!("loading IR at '{}' failed...", path))
    }
  }
}

pub struct ModuleBuilder<'a>
{
  ctx: &'a Context,
  module: CSemiBox<'a, Module>
}

impl<'a> ModuleBuilder<'a>
{
  pub fn build(mut self, opt: JitOptions) -> EngineBuilder<'a> {
    EngineBuilder {
      ctx: self.ctx,
      module: self.module,
      engine: None
    }
  }
}

pub struct EngineBuilder<'a>
{
  ctx: &'a Context,
  module: CSemiBox<'a, Module>,
  engine: Option<JitEngine<'a>>
}

impl<'a> EngineBuilder<'a>
{
  pub fn set_jit(&'a mut self, opt: JitOptions) -> Void {
    self.engine = JitEngine::new(&self.module, opt).ok();
    
    void_ok
  }
  
  pub fn build(mut self) -> JitCompiler<'a>
  {
    JitCompiler {
      ctx: self.ctx,
      module: self.module,
      engine: self.engine.unwrap()
    }
  }
}

pub struct JitCompiler<'a>
{
  ctx: &'a Context,
  module: CSemiBox<'a, Module>,
  engine: JitEngine<'a>
}
