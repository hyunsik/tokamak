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

use std::io::{self, BufWriter, Write};
use llvm::{JitCompiler, Module, ValueRef};
use log::LogLevel;
use rl_sys::readline;

use common::page::{Page, ROWBATCH_SIZE};
use common::page_printer::{ColumnarPagePrinter, PagePrinter};
use common::plugin::PluginManager;
use common::types::HasType;
use common::session::Session;
use exec::NamedSchema;
use exec::processor::MapCompiler;
use parser::lexer::{self, Token};
use parser::parser as p;
use plan::expr::Expr;

// Logical Components
// Repl - Main compoenent for REPL
// SymbolTable - HashMap, keeping functions and variables in top-level module
// IncrementalCompiler - Imitate incremental execution with some tricky way
// ValuePrinter - Print the value contents.

pub struct Repl<'a> {
  // for system
  ctx: ReplContext<'a>,

  // in/out descriptors
  out: &'a mut io::Write, // can be stdout or anything else,
}

pub struct ReplContext<'a> {
  jit: &'a JitCompiler,
  module: Module,
  sess: &'a Session,
  plugin_mgr: &'a PluginManager<'a>,
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

fn is_complete_stmt(tokens: &Vec<Token>) -> bool {
  tokens.len() == 0
}

impl<'a> Repl<'a> {
  pub fn new(sess: &'a Session,
             plugin_mgr: &'a PluginManager<'a>,
             jit: &'a JitCompiler,
             out: &'a mut io::Write)
             -> Repl<'a> {
    Repl {
      ctx: ReplContext {
        jit: jit,
        module: Module::new(jit.context(), "root"),
        sess: sess,
        plugin_mgr: plugin_mgr,
      },
      out: out,
    }
  }

  pub fn print(&mut self, msg: &str) -> &mut Self {
    self.out.write(msg.as_bytes()).ok().unwrap();
    self
  }

  pub fn newline(&mut self) -> &mut Self {
    self.out.write("\n".as_bytes()).ok().unwrap();
    self
  }

  pub fn flush(&mut self) -> &mut Self {
    self.out.flush().ok().unwrap();
    self
  }

  /// for unit test
  pub fn eval_inline(&mut self, line: &str) -> Result<Option<String>, String> {
    self.eval(&mut Vec::new(), &mut Vec::new(), line)
  }

  /// Incrementally parse and eval statements
  ///
  /// The result is set only if parsing and evaluation are completed.
  /// The result is none if statement is incomplete.
  /// The error is set if some error occurs in parsing, code generation or evaluation phase.
  pub fn eval(&mut self,
              tokens: &mut Vec<Token>,
              ast: &mut Vec<Expr>,
              line: &str)
              -> Result<Option<String>, String> {

    tokens.extend(lexer::tokenize(&line));
    let parsed = p::parse(&tokens[..], &ast[..]);

    match parsed {
      Ok((exprs, remain_tokens)) => {
        if is_complete_stmt(&remain_tokens) {
          match Evaluator::eval(&self.ctx, &exprs[0]) {
            Ok(result) => Ok(Some(result)),
            Err(errmsg) => Err(errmsg),
          }
        } else {
          Ok(None)
        }
      }
      Err(msg) => Err(msg),
    }
  }

  /// Loop for read and eval
  pub fn eval_loop(&mut self) {

    let mut linenum = 1usize; // repl line number
    let mut tokens = Vec::new();
    let mut ast = Vec::new();

    loop {
      match readline::readline(&format!("\x1b[33mtkm [{}]> \x1b[0m", linenum)) {
        Ok(Some(line)) => {
          match self.eval(&mut ast, &mut tokens, &line) {
            Ok(Some(result)) => {
              self.print(&result).newline().flush();
              ast.clear();
              tokens.clear();
            }
            Ok(None) => {
              // when statement is incomplete
            }
            Err(msg) => {
              self.print(&msg).newline().flush();
              ast.clear();
              tokens.clear();
            }
          }
        }
        Ok(None) => break, // when eof
        Err(e) => {
          // readline error
          self.print(&format!("{}", e)).newline().flush();
        }
      };

      linenum += 1;
    }
  }
}

pub struct Evaluator;

impl Evaluator {
  pub fn eval(ctx: &ReplContext, expr: &Expr) -> Result<String, String> {
    ctx.jit.add_module(&ctx.module);

    match MapCompiler::compile(ctx.jit,
                               &ctx.module,
                               ctx.plugin_mgr.fn_registry(),
                               ctx.sess,
                               &NamedSchema::new(&[], &[]),
                               &[expr]) {

      Ok((llvm_func, eval_fn)) => {
        let result_ty = expr.ty();
        let in_page = Page::empty_page(0);
        let mut out_page = Page::new(&[result_ty], None);
        let sellist: [usize; ROWBATCH_SIZE] = unsafe { ::std::mem::uninitialized() };
        eval_fn(&in_page, &mut out_page, sellist.as_ptr(), ROWBATCH_SIZE);
        out_page.set_value_count(1); // because of a single expression

        let mut buf: Vec<u8> = Vec::new();
        ColumnarPagePrinter::write(&[result_ty], &out_page, &mut BufWriter::new(&mut buf));

        ctx.jit.remove_module(&ctx.module);
        llvm::delete_func(&llvm_func);
        Ok(String::from_utf8(buf).ok().expect("invalid UTF-8 characters"))
      }
      Err(_) => {
        panic!("Compilation error");
      }
    }
  }
}

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
