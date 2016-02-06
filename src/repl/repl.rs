/// REPL (Read–eval–print loop) executable module provides interactive execution of Tokamak language.

extern crate env_logger;
extern crate libc;
extern crate llvm;
extern crate llvm_sys;
#[macro_use] extern crate log;
extern crate parser;
extern crate rl_sys; // libreadline

extern crate common;
extern crate exec;
extern crate plan;

mod value_print;

use std::collections::HashMap;
use std::io;
use llvm::builder::Builder;
use llvm::JitCompiler;
use llvm::value::{Function, Value};
use rl_sys::readline;

use common::plugin::{FuncRegistry, PluginManager};
use common::types::{HasType, Ty};
use common::session::Session;
use exec::processor::{ExprCompiler, to_llvm_ty};
use plan::expr::Expr;
use parser::lexer;
use parser::parser as p;
use value_print::ValuePrint;

// Logical Components
// Repl - Main compoenent for REPL
// SymbolTable - HashMap, keeping functions and variables in top-level module
// IncCompiler - Imitate incremental execution with some tricky way
// ValuePrinter - Print the value contents.

pub struct Repl<'a> {
  // for system
  jit       : &'a JitCompiler,
  sess      : &'a Session,
  plugin_mgr: &'a PluginManager<'a>,

  // in/out descriptors
  out       : &'a mut io::Write, // can be stdout or anything else,

  // for compile internal
  compiler  : IncCompiler<'a>,
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
             out       : &'a mut io::Write) -> Repl<'a> {
    Repl {
      jit       : jit,
      sess      : sess,
      plugin_mgr: plugin_mgr,

      out       : out,

      compiler  : IncCompiler {
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
  pub fn eval_loop(&mut self) {

    let value_print = ValuePrint::new(self.jit);
    let mut tokens = Vec::new();
    let mut ast = Vec::new();

    loop {
      match readline::readline("\x1b[33mtkm> \x1b[0m") {
        Ok(Some(line)) => {
          tokens.extend(lexer::tokenize(&line));
          let parsed = p::parse(&tokens[..], &ast[..]);

          match parsed {
            Ok(r) => match r.1.len() {
              0 => match self.compiler.compile(&r.0[0]) {
                Ok(f) => {
                  let mut buf = String::new();
                  value_print.print(&f, r.0[0].ty(), &mut buf);
                  self.out.write(buf.as_bytes()).ok().unwrap();
                  self.out.write("\n".as_bytes()).ok().unwrap();
                  self.out.flush().ok().unwrap();
                  ast.clear();
                  tokens.clear();
                  self.jit.delete_func(&f);
                }
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

pub struct IncCompiler<'a> {
  pub jit   : &'a JitCompiler,
  pub sess  : &'a Session,
  pub fn_reg: &'a FuncRegistry,
  pub sym_tb: HashMap<(SymbolKind, String), Symbol>
}


impl<'a> IncCompiler<'a> {

  fn create_fn_proto(&self, bld: &Builder, ret_ty: &Ty) -> Function {
    let jit = self.jit;
    jit.create_func_prototype(
      "processor",
      to_llvm_ty(jit, ret_ty),
      &[jit.get_void_ty()],
      Some(bld))
  }

  fn compile(&self, expr: &Expr) -> Result<Function, String> {
    let bld = self.jit.new_builder();
    let func = self.create_fn_proto(&bld, expr.ty());
    let mut exprc = ExprCompiler::new(self.jit, self.fn_reg, self.sess, &bld);

    match exprc.compile2(&bld, expr) {
      Ok(value)  => {
        bld.create_ret(&value);
        Ok(func)
      }
      Err(_) => Err("".to_string())
    }
  }

  fn build_value_printer(&self) {
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
  env_logger::init().unwrap();

  // Output stream for repl
  let mut stdout = &mut io::stdout();

  // Initialize system context
  let plugin_mgr = &PluginManager::new();
  let jit = &JitCompiler::new_from_bc("../common/target/ir/common.bc").ok().unwrap();
  assert!(jit.get_ty("struct.Chunk").is_some());
  assert!(jit.get_ty("struct.Page").is_some());
  let sess = &Session;

  let mut repl = Repl::new(sess, plugin_mgr, jit, stdout);
  repl.eval_loop();
}