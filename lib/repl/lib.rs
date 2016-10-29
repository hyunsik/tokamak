extern crate env_logger;
#[macro_use] extern crate log;
extern crate rl_sys;
extern crate nix;
extern crate term;

extern crate flang_common as common;
extern crate flang_errors as errors;
extern crate parser;

use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::CString;
use std::io::{self, Write};
use std::rc::Rc;

use nix::libc;
use rl_sys::readline;

mod directive;
use InputType::*;

pub use common::driver::{DriverEnv, ErrorDestination};
use errors::{DiagnosticBuilder, Handler};
use errors::emitter::{ColorConfig, Emitter, EmitterWriter};
use parser::ast::StmtKind::*;
use parser::codemap::CodeMap;
use parser::lexer::{Reader, StringReader};
use parser::parser::{filemap_to_parser, parse_tts_from_source_str, ParseSess, Parser, PResult};
use parser::tokenstream::TokenTree;

static DEFAULT_PROMPT: &'static str = "\x1b[33mflang> \x1b[0m";
static REPL_DUMMY_FILENAME: &'static str = "console.fl";

pub enum InputType<'a> {
  Directive(&'a str, Vec<&'a str>),
  ExternalComamnd(&'a str),
  ExecuteSource(&'a str),
  Empty,
}

pub enum ReplAction {
  Done,
  Continue(String),
  Quit
}

pub struct Repl {
  src_file: SourceFile,
  parsess: ParseSess,
  env: DriverEnv, // stream err,
}

impl Repl {
  pub fn new(env: DriverEnv) -> Repl {
    Repl {
      src_file: SourceFile::new(),
      parsess: ParseSess::new(),
      env: env,
    }
  }

  fn println(&self, msg: &[u8]) {
    self.env.errdst.borrow_mut().write(msg).ok();
  }

  pub fn run(&mut self) {
    self.println(
      b"Welcome to Flang version 0.1. (Type :help for assistance.)\n");

    let mut prompt: String = DEFAULT_PROMPT.to_string();

    loop {
      match readline::readline(&prompt) {
        Ok(Some(line)) => {
          match self.handle_line(&line) {
            ReplAction::Done => { prompt = DEFAULT_PROMPT.to_string(); }
            ReplAction::Continue(p) => prompt = p,
            ReplAction::Quit => break
          }
        }
        Ok(None) => break, // eof
        Err(msg) => {
          self.println(format!("ERROR: {}", msg).as_bytes());
        }
      }
    }
  }

  fn handle_line(&mut self, line: &str) -> ReplAction {
    match self.parse_input(line) {
      Directive(d, args) => self.exec_directive(d, args),
      ExecuteSource(line) => self.exec_line(line),
      ExternalComamnd(command) => self.exec_external_program(command),
      Empty => ReplAction::Done // just enter without any type
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

  pub fn exec_directive(&self, directive: &str, args: Vec<&str>) -> ReplAction {
    match directive {
      "dump" => {
        let bytes = self.src_file.as_bytes();
        self.println(bytes);
        ReplAction::Done
      }
      "quit" => ReplAction::Quit,
      _ => ReplAction::Done
    }
  }

  fn emitter(&self, cm: Rc<CodeMap>) -> Box<Emitter> {
    match *self.env.errdst.borrow() {
      ErrorDestination::Stderr => {
        Box::new(EmitterWriter::stderr(ColorConfig::Auto, Some(cm)))
      }
      ErrorDestination::Raw(ref buf) => {
        let delegator = WriteDelegator {write: buf.clone()};
        Box::new(EmitterWriter::new(Box::new(delegator), Some(cm)))
      }
    }
  }

  pub fn exec_line(&mut self, line: &str) -> ReplAction {
    let cm = Rc::new(CodeMap::new());
    let emitter = self.emitter(cm.clone());
    let handler = Handler::with_emitter(true, false, emitter);
    let parsess = ParseSess::with_span_handler(handler, cm.clone());

    let mut parser = parse_flang(&parsess, line);
    self.src_file.add_line(line);

    match parser.parse_item() {
      Ok(Some(item)) => {
        println!("item");
        return ReplAction::Done
      }
      Ok(None) => println!("no item"),
      Err(ref mut e) => {
        println!("not item");
        e.cancel();
      }
    };

    match parser.parse_full_stmt() {
      Ok(Some(stmt)) => {
        match stmt.node {
          Local(l) => println!("local"),
          Item(i) => println!("item"),
          Expr(e) => println!("expr"),
          Semi(s) => println!("semi"),
        }
      }
      Ok(None) => println!("no stmt"),
      Err(e) => println!("not stmt"),
    };

    ReplAction::Done

    // append source
    // generate mir
    // check ast error
    // add source which is valid
    // skip if the source doesn't need to run
    // generate llvm ir and module
    // reorganize llvm module
  }

  pub fn exec_external_program(&self, command: &str) -> ReplAction {
    let c_to_print = CString::new(command).unwrap();
    unsafe { libc::system(c_to_print.as_ptr()); }
    ReplAction::Done
  }
}

fn parse_flang<'a>(parsess: &'a ParseSess, line: &str) -> Parser<'a> {
  let filemap = parsess.codemap().new_filemap_and_lines("console.fl", None, line);
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
}