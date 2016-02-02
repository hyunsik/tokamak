#![feature(libc)]
extern crate common;
extern crate exec;
extern crate plan;
extern crate libc;
extern crate llvm;
extern crate parser;
extern crate readline;

use common::plugin::FuncRegistry;
use common::types::Ty;
use common::session::Session;
use exec::processor::ExprCompiler;
use llvm::JitCompiler;
use llvm::value::Value;
use plan::expr::Expr;
use parser::lexer;
use parser::parser as p;
use readline::*;

// Keep
pub struct ReplContext<'a> {
  pub jit   : &'a JitCompiler,
  pub fn_reg: &'a FuncRegistry,
  pub sess  : &'a Session
}

// Execute the completed AST, then 
// * return a value if expression
// * return an empty value if statement
// * return error if any error is included  
pub struct IncrementalExecutor;

// TODO - Error should include span and error message.

impl IncrementalExecutor {
  pub fn exec1<'a>(ctx: &'a ReplContext, line: &'a Expr) -> Result<Option<(&'a Ty, Value)>, String> {
    Err("".to_string())
    
    let exprc = ExprCompiler::new(ctx.jit, ctx.fn_reg, ctx.sess);
  }
}

// Print a value according to the Type
pub struct ValuePrinter;

pub fn main() {
  unsafe {
    let mut ast = Vec::new();

    loop {
      let line = readline(from_str("\x1b[33mtkm> \x1b[0m"));
      if line.is_null() {
        break;
      }

      let tokens = lexer::tokenize(to_str(line));
      let parsed = p::parse(&tokens[..], &ast[..]);

      match parsed {
        Ok(r) => {
          match r.1.len() {
            0 => {
              println!("{:?}, {:?}", r.0, r.1);
              add_history(line);
            }
            _ => {}
          }
        }
        Err(msg) => {println!("{}", msg)}
      }
    }
  }
}