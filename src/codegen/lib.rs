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

pub struct JitCompiler<'a> 
{
  ctx   : Option<CBox<Context>>,
  module: Option<CSemiBox<'a, Module>>,
  ee    : Option<JitEngine<'a>>,
  
  _marker: PhantomData<&'a ()>
}

impl<'a> JitCompiler<'a>
{
  pub fn new() -> JitCompiler<'a>
  {
    JitCompiler {      
      ctx    : Some(Context::new()),
      module : None,
      ee     : None,
      
      _marker: PhantomData
    }
  }
  
  pub fn init_module(&'a mut self) -> Void
  {    
    self.module = Some(Module::new("xxx", self.ctx.as_ref().unwrap()));      
    self.ee     = Some(JitEngine::new((self.module.as_ref()).unwrap(), JitOptions {opt_level: JIT_OPT_LVEL}).unwrap());
    
    void_ok
  }

  pub fn load_from_bitcode(&'a mut self, path: &str) -> Void
  {    
    self.module = Some(Module::parse_bitcode(self.ctx.as_ref().unwrap(), path)
      .expect(&format!("loading {} failed...", path)));
    self.ee     = Some(JitEngine::new((self.module.as_ref()).unwrap(), JitOptions {opt_level: JIT_OPT_LVEL}).unwrap());
    
    void_ok
  }
}

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
  pub fn build(&'a mut self, opt: JitOptions) {
    self.engine = JitEngine::new(&self.module, opt).ok();
  }
}
