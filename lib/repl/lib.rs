#![feature(set_stdio)]

extern crate env_logger;
#[macro_use] extern crate log;
extern crate rl_sys;
extern crate nix;
extern crate term;

extern crate flang_common as common;
extern crate flang_compiler as compiler;
extern crate flang_errors as errors;
extern crate parser;

use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::CString;
use std::io::{self, Write};
use std::sync::mpsc::channel;
use std::panic::catch_unwind;
use std::rc::Rc;
use std::str;
use std::sync::{Arc, Mutex};
use std::thread;

use nix::libc;
use rl_sys::readline;

mod directive;
use InputType::*;
use ErrorKind::*;

pub use common::driver::{DriverEnv, ErrorDestination};
use compiler::{IncrCompilerAction, IncrCompiler};
use errors::{DiagnosticBuilder, Handler};
use errors::emitter::{ColorConfig, Emitter, EmitterWriter};
use parser::ast::Stmt;
use parser::ast::StmtKind::*;
use parser::codemap::CodeMap;
use parser::lexer::{Reader, StringReader};
use parser::parser::{filemap_to_parser, parse_tts_from_source_str, ParseSess, Parser, PResult};
use parser::tokenstream::TokenTree;

static WELCOME_MSG: &'static str =
  "Welcome to Flang version 0.1. (Type :help for assistance.)\n";
static DEFAULT_PROMPT: &'static str = "\x1b[33mflang> \x1b[0m";
static CONTINUE_PROMPT: &'static str = "\x1b[33m> \x1b[0m";
static REPL_DUMMY_FILENAME: &'static str = "console.fl";

pub enum InputType<'a> {
  Directive(&'a str, Vec<&'a str>),
  ExternalComamnd(&'a str),
  ExecuteSource(&'a str),
  Empty,
}

pub struct Repl {
  unexecuted_src: SourceFile,
  executed_src: SourceFile,
  compiler: IncrCompiler,
  env: DriverEnv,
}

impl Repl {
  pub fn new(env: DriverEnv) -> Repl {
    Repl {
      unexecuted_src: SourceFile::new(),
      executed_src: SourceFile::new(),
      compiler: IncrCompiler::new(
        REPL_DUMMY_FILENAME.to_owned(),
        env.errdst.clone()),
      env: env,
    }
  }

  fn println(&self, msg: &str) {
    self.env.errdst.borrow_mut().write(msg.as_bytes()).ok();
  }

  pub fn run(&mut self) {
    self.println(WELCOME_MSG);

    let mut prompt: String = DEFAULT_PROMPT.to_string();

    loop {
      match readline::readline(&prompt) {
        Ok(Some(line)) => {
          match self.handle_line(&line) {
            IncrCompilerAction::Done | IncrCompilerAction::Error => {
              prompt = DEFAULT_PROMPT.to_string();
            }
            IncrCompilerAction::Continue => prompt = CONTINUE_PROMPT.to_string(),
            IncrCompilerAction::Quit => break
          }
        }
        Ok(None) => break, // eof
        Err(msg) => {
          self.println(&format!("ERROR: {}", msg));
        }
      }
    }
  }

  fn handle_line(&mut self, line: &str) -> IncrCompilerAction {
    match self.parse_input(line) {
      Directive(d, args) => self.exec_directive(d, args),
      ExecuteSource(line) => self.exec_line(line),
      ExternalComamnd(command) => self.exec_external_program(command),
      Empty => IncrCompilerAction::Done // just enter without any type
    }
  }

  fn parse_input<'a>(&self, line: &'a str) -> InputType<'a> {
    let mut tokens = line.split_whitespace();
    let first_token = tokens.next();

    match first_token {
      Some(first) => match first {
        x if x.starts_with(':') => {
          Directive(&first[1..], tokens.collect::<Vec<_>>())
        }
        x if x.starts_with('!') => ExternalComamnd(&line[1..]),
        _ => ExecuteSource(line)
      },
      None => Empty
    }
  }

  pub fn exec_directive(&self, directive: &str, args: Vec<&str>)
      -> IncrCompilerAction {
    match directive {
      "dump" => {
        self.println(self.executed_src.as_str());
        IncrCompilerAction::Done
      }
      "quit" => IncrCompilerAction::Quit,
      _ => IncrCompilerAction::Done
    }
  }

  fn new_emitter(errdst: ErrorDestination, cm: Rc<CodeMap>)
      -> (Box<Emitter>, Rc<RefCell<Vec<ErrorKind>>>) {
    let ew = match errdst {
      ErrorDestination::Stderr => {
        EmitterWriter::stderr(ColorConfig::Auto, Some(cm))
      }
      ErrorDestination::Raw(ref buf) => {
        let delegator = WriteDelegator {write: buf.clone()};
        EmitterWriter::new(Box::new(delegator), Some(cm))
      }
    };

    let (emitter, errkinds) = EmitterDelegator::new(ew);
    (Box::new(emitter), errkinds)
  }

  pub fn exec_line(&mut self, line: &str) -> IncrCompilerAction {

    self.unexecuted_src.add_line(line);
    let to_be_executed = self.unexecuted_src.as_str().to_owned();

    let action = self.compiler.eval(&to_be_executed);

    match action {
      IncrCompilerAction::Error => {
        self.unexecuted_src.clear();
      }

      IncrCompilerAction::Done => {
        self.executed_src.add_line(&to_be_executed);
      }

      _ => {}
    };

    action
  }

  pub fn exec_external_program(&self, command: &str) -> IncrCompilerAction {
    let c_to_print = CString::new(command).unwrap();
    unsafe { libc::system(c_to_print.as_ptr()); }
    IncrCompilerAction::Done
  }
}

fn parse_flang<'a>(parsess: &'a ParseSess, line: &str) -> Parser<'a> {
  let filemap = parsess
    .codemap()
    .new_filemap_and_lines(REPL_DUMMY_FILENAME, None, line);
  filemap_to_parser(&parsess, filemap)
}

pub struct WriteDelegator {
  write: Rc<RefCell<Box<io::Write + Send>>>
}

unsafe impl Send for WriteDelegator {}

impl io::Write for WriteDelegator {
  fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
    self.write.borrow_mut().write(buf)
  }

  fn flush(&mut self) -> io::Result<()> {
    self.write.borrow_mut().flush()
  }
}

pub struct SourceFile {
  source: String
}

impl SourceFile {
  pub fn new() -> SourceFile {
    SourceFile { source : String::new() }
  }

  pub fn add_line(&mut self, line: &str) {
    self.source.push_str(line);
    self.source.push_str("\n");
  }

  pub fn as_str(&self) -> &str {
    &self.source
  }

  pub fn as_bytes(&self) -> &[u8] {
    self.source.as_bytes()
  }

  pub fn clear(&mut self) {
    self.source.clear();
  }

  pub fn update(&mut self, lines: &str) {
    self.clear();
    self.source.push_str(lines);
  }
}

unsafe impl Sync for SourceFile {}

enum ErrorKind {
  UnclosedDelimiter,
  Unknown
}

struct EmitterDelegator {
  em: EmitterWriter,
  errors: Rc<RefCell<Vec<ErrorKind>>>
}

impl EmitterDelegator {
  pub fn new(em : EmitterWriter)
      -> (EmitterDelegator, Rc<RefCell<Vec<ErrorKind>>>) {
    let errors = Rc::new(RefCell::new(Vec::new()));
    let delegator = EmitterDelegator { em : em, errors: errors.clone() };
    (delegator, errors)
  }
}

impl Emitter for EmitterDelegator {
  fn emit(&mut self, db: &DiagnosticBuilder) {

    let errkind = error_kind(db.message());

    match errkind {
      UnclosedDelimiter => {},
      _ => self.em.emit(db)
    };

    self.errors.borrow_mut().push(errkind);
  }
}

fn error_kind(msg: &str) -> ErrorKind {
  if is_unclosed_delemiter(msg) {
    ErrorKind::UnclosedDelimiter
  } else {
    Unknown
  }
}

fn is_unclosed_delemiter(msg: &str) -> bool {
  msg == "this file contains an un-closed delimiter"
}