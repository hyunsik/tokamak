#![feature(set_stdio)]

extern crate crossbeam;
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
use compiler::IncrCompilerAction;
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
  env: DriverEnv, // stream err,
}

impl Repl {
  pub fn new(env: DriverEnv) -> Repl {
    Repl {
      unexecuted_src: SourceFile::new(),
      executed_src: SourceFile::new(),
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

  fn need_more_liens(errs: &Rc<RefCell<Vec<ErrorKind>>>) -> bool {
    errs.borrow().len() == 1 && match errs.borrow()[0] {
      UnclosedDelimiter => true,
      _ => false
    }
  }

  fn is_error(errs: &Rc<RefCell<Vec<ErrorKind>>>) -> bool {
    if errs.borrow().len() == 0 {
      return false;
    }

    if errs.borrow().len() > 2 {
      return true;
    }

    // only if errors.len() == 1
    match errs.borrow()[0] {
      UnclosedDelimiter => false,
      _ => true
    }
  }

  pub fn exec_line(&mut self, line: &str) -> IncrCompilerAction {

    // Temporarily have stack size set to 16MB to deal with nom-using crates failing
    const STACK_SIZE: usize = 16 * 1024 * 1024; // 16MB

    struct Sink(Arc<Mutex<Vec<u8>>>);
    impl Write for Sink {
        fn write(&mut self, data: &[u8]) -> io::Result<usize> {
            Write::write(&mut *self.0.lock().unwrap(), data)
        }
        fn flush(&mut self) -> io::Result<()> {
            Ok(())
        }
    }

    let data = Arc::new(Mutex::new(Vec::new()));
    let err = Sink(data.clone());

    //let (tx, rx) = channel();
    let line = line.to_owned();
    let mut unexecuted_src = self.unexecuted_src.as_str().to_owned();
    let errdst = self.env.errdst.borrow().clone();

    let task = thread::Builder::new().name("parsing".to_owned());
    let handle = task.spawn(move || {
      io::set_panic(Some(Box::new(err)));

      unexecuted_src.push_str(&line);
      unexecuted_src.push_str("\n");

      let cm = Rc::new(CodeMap::new());
      let (emitter, errs) = Repl::new_emitter(errdst, cm.clone());
      let handler = Handler::with_emitter(true, false, emitter);
      let parsess = ParseSess::with_span_handler(handler, cm.clone());
      let mut parser = parse_flang(&parsess, &unexecuted_src);

      if Repl::need_more_liens(&errs) {
        println!("need more lines");
        return (IncrCompilerAction::Continue, unexecuted_src, "".to_owned());
      } else if Repl::is_error(&errs) {
        return (IncrCompilerAction::Error, "".to_owned(), "".to_owned());
      }


      let item = match parser.parse_item() {
        Ok(item) => item,
        Err(ref mut e) => {
          e.cancel();
          None
        }
      };

      let stmt = if item.is_none() {
        match parser.parse_full_stmt() {
          Ok(stmt) => stmt,
          Err(_) => None,
        }
      } else {
        None
      };

      (IncrCompilerAction::Done, "".to_owned(), unexecuted_src.to_owned())
    }).unwrap();

    let (action, unexecuted_src, executed_src) = match handle.join() {
      Ok(result) => result,

      Err(value) => {
        // if it is a real error or bug, it will print out the stacktrace
        if !value.is::<errors::FatalError>() {
          writeln!(io::stderr(), "{}",
            str::from_utf8(&data.lock().unwrap()).unwrap()).unwrap();
        }
        (IncrCompilerAction::Error, "".to_owned(), "".to_owned())
      }
    };

    self.unexecuted_src.update(&unexecuted_src);
    self.executed_src.update(&executed_src);

    // append source
    // generate mir
    // check ast error
    // add source which is valid
    // skip if the source doesn't need to run
    // generate llvm ir and module
    // reorganize llvm module

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