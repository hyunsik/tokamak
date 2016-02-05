/// REPL (Read–eval–print loop) executable module provides interactive execution of Tokamak language.

extern crate common;
extern crate exec;
extern crate plan;
extern crate libc;
extern crate llvm;
extern crate llvm_sys;
extern crate parser;
extern crate rl_sys; // libreadline

use std::collections::HashMap;
use std::io;
use common::plugin::{FuncRegistry, PluginManager};
use common::types::{HasType, Ty};
use common::session::Session;
use exec::processor::ExprCompiler;
use llvm::JitCompiler;
use llvm::value::Value;
use plan::expr::Expr;
use parser::lexer;
use parser::parser as p;
use rl_sys::readline;

mod value_printer;

// Logical Components
// Repl - Main compoenent for REPL
// SymbolTable - HashMap, keeping functions and variables in top-level module
// IncrementalCompiler - Imitate incremental execution with some tricky way
// ValuePrinter - Print the value contents.

pub struct Repl<'a> {
  // for system
  sess      : &'a Session,
  plugin_mgr: &'a PluginManager<'a>,
  // in/out descriptors
  out       : &'a io::Write, // can be stdout or anything else,

  // for compile internal
  compiler  : IncrementalCompiler2<'a>,
}

#[derive(Eq, Hash, PartialEq)]
pub enum SymbolKind {
  // Function
  Func,
  // Immutable value
  ImmVal,
  // Mutable value
  MutVal
}

pub struct Symbol;

impl<'a> Repl<'a> {
  pub fn new(sess      : &'a Session,
             plugin_mgr: &'a PluginManager<'a>,
             jit       : &'a JitCompiler,
             out       : &'a io::Write) -> Repl<'a> {
    Repl {
      out       : out,
      sess      : sess,
      plugin_mgr: plugin_mgr,

      compiler  : IncrementalCompiler2 {
        jit: jit,
        fn_reg: plugin_mgr.fn_registry(),
        sess: sess,
        sym_tb: HashMap::new()
      }
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
  pub fn eval_loop(&self) {
    unsafe {
      let mut tokens = Vec::new();
      let mut ast = Vec::new();

      loop {
        let line = match readline::readline("\x1b[33mtkm> \x1b[0m") {
          Ok(Some(line)) => {
            tokens.extend(lexer::tokenize(&line));
            let parsed = p::parse(&tokens[..], &ast[..]);

            match parsed {
              Ok(r) => match r.1.len() {
                0 => match self.compiler.exec1(&r.0[0]) {
                  Ok(Some((ty, ref val))) => {
                    ValuePrinter::print(ty, val);
                    ast.clear();
                    tokens.clear();
                  }
                  Ok(None) => {}
                  Err(msg) => {
                    println!("{}", msg);
                    ast.clear();
                    tokens.clear();
                  }
                },
                _ => {}
              },
              Err(msg) => {println!("{}", msg)}
            }
          }
          Ok(None)       => { break }
          Err(e)         => { println!("{}", e) }
        };
        //add_history(line);
      }
    }
  }
}

pub struct IncrementalCompiler2<'a> {
  pub jit   : &'a JitCompiler,
  pub sess  : &'a Session,
  pub fn_reg: &'a FuncRegistry,
  pub sym_tb: HashMap<(SymbolKind, String), Symbol>
}


impl<'a> IncrementalCompiler2<'a> {
  pub fn exec1<'b>(&self, expr: &'b Expr) -> Result<Option<(&'b Ty, Value)>, String> {
    let bld = self.jit.new_builder();
    let mut exprc = ExprCompiler::new(self.jit, self.fn_reg, self.sess, &bld);

    match exprc.compile2(&bld, expr) {
      Ok(v)  => Ok( Some((expr.ty(), v)) ),
      Err(_) => Err("".to_string())
    }
  }
}

// Execute the completed AST, then
// * return a value if expression
// * return an empty value if statement
// * return error if any error is included

// TODO - Error should include span and error message.

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

  // Output stream for repl
  let mut stdout = &mut io::stdout();

  // Initialize system context
  let plugin_mgr = &PluginManager::new();
  let jit = &JitCompiler::new_from_bc("../common/target/ir/common.bc").ok().unwrap();
  assert!(jit.get_ty("struct.Chunk").is_some());
  assert!(jit.get_ty("struct.Page").is_some());
  let sess = &Session;

  let repl = Repl::new(sess, plugin_mgr, jit, stdout);
  repl.eval_loop();
}