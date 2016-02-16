//! REPL (Read–eval–print loop) executable module provides interactive
//! execution of Tokamak language.

extern crate env_logger;
extern crate libc;
extern crate llvm;
extern crate llvm_sys;
#[macro_use]
extern crate log;
extern crate parser;
extern crate rl_sys; // libreadline

extern crate common;
extern crate exec;
extern crate plan;

use std::collections::HashMap;
use std::io;
use llvm::{Builder, Function, JitCompiler, Module, ValueRef, Verifier};
use rl_sys::readline;

use common::page::{Page, ROWBATCH_SIZE};
use common::plugin::{FuncRegistry, PluginManager};
use common::types::{HasType, Ty};
use common::session::Session;
use exec::{NamedSchema, ColumnarRowPrinter, RowPrinter};
use exec::processor::{MapCompiler, ExprCompiler, to_llvm_ty};
use plan::expr::Expr;
use parser::lexer;
use parser::parser as p;

// Logical Components
// Repl - Main compoenent for REPL
// SymbolTable - HashMap, keeping functions and variables in top-level module
// IncrementalCompiler - Imitate incremental execution with some tricky way
// ValuePrinter - Print the value contents.

pub struct Repl<'a> {
  // for system
  jit: &'a JitCompiler,
  module: Module,
  sess: &'a Session,
  plugin_mgr: &'a PluginManager<'a>,

  // in/out descriptors
  out: &'a mut io::Write, // can be stdout or anything else,
}

#[derive(Eq, Hash, PartialEq)]
pub enum SymbolKind {
  // Function
  Func,
  // Immutable value
  ImmVal,
  // Mutable value
  MutVal,
}

pub struct Symbol;

impl<'a> Repl<'a> {
  pub fn new(sess: &'a Session,
             plugin_mgr: &'a PluginManager<'a>,
             jit: &'a JitCompiler,
             out: &'a mut io::Write)
             -> Repl<'a> {
    Repl {
      jit: jit,
      module: Module::new(jit.context(), "root"),
      sess: sess,
      plugin_mgr: plugin_mgr,
      out: out
    }
  }

  /// Execute a statement or expression (public for testing)
  pub fn execute(stmts: &str) -> Result<(), String> {
    Ok(())
  }

  /// Execute a meta command (public for testing)
  pub fn run_metacmd(cmd: &str) -> Result<(), String> {
    Ok(())
  }

  /// Loop for read and eval
  pub fn eval_loop(&mut self) {

    let mut tokens = Vec::new();
    let mut ast = Vec::new();

    loop {
      match readline::readline("\x1b[33mtkm> \x1b[0m") {
        Ok(Some(line)) => {
          tokens.extend(lexer::tokenize(&line));
          let parsed = p::parse(&tokens[..], &ast[..]);

          self.jit.add_module(&self.module);

          match parsed {
            Ok((exprs, remain_tokens)) => {
              match remain_tokens.len() {
                0 => {
                  match MapCompiler::compile(self.jit,
                          &self.module,
                          self.plugin_mgr.fn_registry(),
                          self.sess,
                          &NamedSchema::new(&[], &[]),
                          &[&exprs[0]]) {
                    Ok((llvm_func, map)) => {
                      let result_ty = exprs[0].ty();

                      /*
                      debug!("{:?}", r.0[0]);
                      if log_enabled!(::log::LogLevel::Debug) {
                        f.dump();
                      }
                      Verifier::verify_func(f).unwrap_or_else(|err_msg| {
                        if !log_enabled!(::log::LogLevel::Debug) {
                          f.dump();
                        }
                        panic!("{}", err_msg);
                      });*/

                      let in_page = Page::empty_page(0);
                      let mut out_page = Page::new(&[result_ty], None);
                      let sellist: [usize; ROWBATCH_SIZE] = unsafe { ::std::mem::uninitialized() };
                      map(&in_page, &mut out_page, sellist.as_ptr(), ROWBATCH_SIZE);
                      out_page.set_value_count(1);

                      ColumnarRowPrinter::write(&[result_ty], &out_page, self.out);
                      self.out.write("\n".as_bytes()).ok().unwrap();
                      self.out.flush().ok().unwrap();

                      self.jit.remove_module(&self.module);
                      llvm::delete_func(&llvm_func);
                      ast.clear();
                      tokens.clear();
                    }
                    Err(msg) => {
                      panic!("error");
                      ast.clear();
                      tokens.clear();
                    }
                  }
                }
                _ => {}
              }
            }
            Err(msg) => println!("{}", msg),
          }
        }
        Ok(None) => break,
        Err(e) => println!("{}", e),
      };
      // add_history(line);
    }
  }
}

/// Evaluator: Expr -> String
/// ValuePrint: Value -> String
/// IncCompiler: Expr -> Value

// Execute the completed AST, then
// * return a value if expression
// * return an empty value if statement
// * return error if any error is included

// TODO - Error should include span and error message.

pub fn main() {
  env_logger::init().unwrap();

  // Output stream for repl
  let mut stdout = &mut io::stdout();

  // Initialize system context
  let plugin_mgr = &PluginManager::new();
  let jit = &JitCompiler::from_bc("../common/target/ir/common.bc").ok().unwrap();
  assert!(jit.get_ty("struct.Chunk").is_some());
  assert!(jit.get_ty("struct.Page").is_some());
  let sess = &Session;

  let mut repl = Repl::new(sess, plugin_mgr, jit, stdout);
  repl.eval_loop();
}
