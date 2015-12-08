//! JIT Compiler to generate code fragments in runtime.

#![feature(libc)]
extern crate libc;
extern crate llvm;

extern crate common;

use std::borrow::Borrow;
use std::marker::PhantomData;

use llvm::*;

use common::err::{Void, void_ok};

pub struct JitCompiler<'a> 
{
  ctx   : CBox<Context>,
  module: Option<CSemiBox<'a, Module>>,
  ee    : Option<JitEngine<'a>>,
  
  _marker: PhantomData<&'a ()>
}

struct JitCompilerBuilder<'a>
{
  ctx   : CBox<Context>,
  module: Option<CSemiBox<'a, Module>>,
  ee    : Option<JitEngine<'a>>,
  _marker: PhantomData<&'a ()>
}

impl<'a> JitCompilerBuilder<'a>
{
  pub fn init_module(&'a mut self, name: &str) -> &'a mut JitCompilerBuilder 
  {
    self.module = Some(Module::new(name, self.ctx.borrow()));
    self.ee     = Some(JitEngine::new((self.module.as_ref()).unwrap(), JitOptions {opt_level: 0}).unwrap());
    
    self
  }
}

/*
impl<'a> JitCompiler<'a>
{
  pub fn new() -> JitCompilerBuilder<'a>
  {
    JitCompiler {      
      ctx    : Context::new(),
      module : None,
      ee     : None,
      
      _marker: PhantomData
    }
  }

  pub fn load_from_bitcode(&'a mut self, path: &str) -> Void
  {    
    self.module = Some(Module::parse_bitcode(self.ctx.borrow(), path)
      .expect(&format!("loading {} failed...", path)));
    self.ee     = Some(JitEngine::new((self.module.as_ref()).unwrap(), JitOptions {opt_level: 0}).unwrap());
    
    void_ok
  }
}
*/