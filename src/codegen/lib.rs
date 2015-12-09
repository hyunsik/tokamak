//! JIT Compiler to generate code fragments in runtime.

#![feature(libc)]
extern crate libc;
extern crate llvm;

extern crate common;

use llvm::*;

use common::err::{Void, void_ok};

pub const JIT_OPT_LVEL: usize = 2;

pub struct JitEngineBuilder<'a>
{
  ctx: &'a Context,
}

impl<'a> JitEngineBuilder<'a>
{
  pub fn new(ctx: &'a Context) -> JitEngineBuilder<'a>
  {
    JitEngineBuilder {
      ctx: ctx      
    }
  }
  
  pub fn new_module(self, name: &str) -> ModuleBuilder<'a> 
  {
    ModuleBuilder {
      ctx: self.ctx,
      module: Module::new(name, self.ctx)
    }
  }
  
  pub fn from_bitcode(self, path: &str) -> ModuleBuilder<'a> 
  {
    ModuleBuilder {
      ctx: self.ctx,
      module: Module::parse_bitcode(self.ctx, path)
                .expect(&format!("loading bitcode at '{}' failed...", path))
    }
  }
  
  pub fn from_ir(self, path: &str) -> ModuleBuilder<'a> 
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
  pub fn optimize(self, opt_level: usize, size_level: usize) 
      -> ModuleBuilder<'a>
  {
    self.module.optimize(opt_level, size_level);
    self
  }
  
  pub fn set_target(self, target: &str) -> ModuleBuilder<'a>
  {
    self.module.set_target(target);
    self
  }
  
  pub fn set_data_layout(self, layout: &str) -> ModuleBuilder<'a>
  {
    self.module.set_data_layout(layout);
    self
  }
  
  pub fn build(mut self) -> EngineBuilder<'a> 
  {
    EngineBuilder {
      ctx: self.ctx,
      module: self.module,
      jit_opt: None,
      engine: None      
    }
  }
}

pub struct EngineBuilder<'a>
{
  ctx    : &'a Context,
  module : CSemiBox<'a, Module>,  
  engine : Option<JitEngine<'a>>,
  jit_opt: Option<JitOptions>,
}

impl<'a> EngineBuilder<'a>
{
  pub fn set_opt_level(mut self, opt: JitOptions) -> EngineBuilder<'a> {
    if let Some(opt) = self.jit_opt {
      panic!("EngineBuilder::set_opt_level should be called before create()");
    }
    
    self.jit_opt = Some(opt);    
    self
  }
  
  pub fn create(&'a mut self) {
    self.engine = JitEngine::new(&self.module, self.jit_opt.unwrap()).ok();
  }
  
  pub fn add_module(&'a self, m: &'a Module) -> &'a EngineBuilder
  {
    let ee = self.engine.as_ref().expect("Must be create() before add_module()");
    ee.add_module(m);    
    self
  }
  
  pub fn build(mut self) -> JitCompiler<'a>
  {
    JitCompiler {
      ctx: self.ctx,
      module: self.module,
      builder: Builder::new(self.ctx),
      engine: self.engine.unwrap()
    }
  }
}

pub struct JitCompiler<'a>
{
  ctx    : &'a Context,
  module : CSemiBox<'a, Module>,
  builder: CSemiBox<'a, Builder>,
  engine : JitEngine<'a>
}

impl<'a> JitCompiler<'a>
{
  pub fn context(&'a self) -> &'a Context { self.ctx }
  
  pub fn module(&'a self) -> &'a Module { &self.module }
  
  pub fn builder(&'a self) -> &'a Builder { &self.builder }
  
  pub fn engine(&'a self) -> &'a JitEngine { &self.engine }
}

#[cfg(test)]
mod tests {
  use llvm::Context;
  use super::JitCompiler;
  use super::JitEngineBuilder;
  use super::ModuleBuilder;
  
  #[test]
  fn test() {
    let ctx = Context::new();
    let x = JitEngineBuilder::new(&ctx)
      .new_module("test1")
      .build();
    // let y = x.new_module("test1");
    // let m: ModuleBuilder = JitEngineBuilder::new(&ctx)
    //                          .new_module("test1");
  }
}