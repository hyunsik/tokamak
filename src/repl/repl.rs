#![feature(libc)]
extern crate common;
extern crate exec;
extern crate plan;
extern crate libc;
extern crate llvm;
extern crate llvm_sys;
extern crate parser;
extern crate readline;

use common::plugin::{FuncRegistry, PluginManager};
use common::types::{HasType, Ty};
use common::session::Session;
use exec::processor::ExprCompiler;
use llvm::JitCompiler;
use llvm::value::Value;
use llvm_sys::execution_engine::LLVMGenericValueToInt;
use plan::expr::Expr;
use parser::lexer;
use parser::parser as p;
use readline::*;

mod value_printer;

// Keep
pub struct ReplContext<'a> {
  pub jit   : &'a JitCompiler,
  pub fn_reg: &'a FuncRegistry,
  pub sess  : &'a Session
}

impl<'a> ReplContext<'a> {
  pub fn new(jit   : &'a JitCompiler, 
             fn_reg: &'a FuncRegistry, 
             sess  : &'a Session) -> ReplContext<'a> {                   
    ReplContext {
      jit   : jit,
      fn_reg: fn_reg,
      sess  : sess
    }
  }
}

// Execute the completed AST, then 
// * return a value if expression
// * return an empty value if statement
// * return error if any error is included  
pub struct IncrementalExecutor;

// TODO - Error should include span and error message.

impl IncrementalExecutor {
  pub fn exec1<'a>(ctx: &'a ReplContext, expr: &'a Expr) -> Result<Option<(&'a Ty, Value)>, String> {    
    let bld = ctx.jit.new_builder();
    let mut exprc = ExprCompiler::new(ctx.jit, ctx.fn_reg, ctx.sess, &bld);
    
    match exprc.compile2(&bld, expr) {
      Ok(v)  => Ok( Some((expr.ty(), v)) ),
      Err(_) => Err("".to_string()) 
    }   
  }
}

// Print a value according to the Type
pub struct ValuePrinter;

impl ValuePrinter {
  pub fn print(ty: &Ty, val: &Value) {
    match *ty {
      Ty::Bool => {}
      Ty::I64  => {println!("x {}", val)}
      _        => panic!("Unknown")      
    }
  }
}

pub fn main() {
  let plugin_mgr = PluginManager::new();
  let jit = JitCompiler::new_from_bc("../common/target/ir/common.bc").ok().unwrap();
  assert!(jit.get_ty("struct.Chunk").is_some());
  assert!(jit.get_ty("struct.Page").is_some());
  let sess = Session;
    
  unsafe {    
    let repl_ctx = ReplContext::new(&jit, plugin_mgr.fn_registry(), &sess);
    
    let mut ast = Vec::new();

    loop {
      let line = readline(from_str("\x1b[33mtkm> \x1b[0m"));
      if line.is_null() {
        break;
      }

      let tokens = lexer::tokenize(to_str(line));
      let parsed = p::parse(&tokens[..], &ast[..]);

      match parsed {
        Ok(r) => match r.1.len() {
          0 => match IncrementalExecutor::exec1(&repl_ctx, &r.0[0]) {
            Ok(Some((ty, ref val))) => ValuePrinter::print(ty, val),
            Ok(None) => {},
            Err(msg) => println!("e1 {}", msg)
          },
          _ => {}
        },
        Err(msg) => {println!("e2 {}", msg)}
      }
      
      add_history(line);
    }
  }
}