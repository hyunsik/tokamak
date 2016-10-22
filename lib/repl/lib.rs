extern crate env_logger;
#[macro_use] extern crate log;
extern crate rl_sys;
extern crate nix;


use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::CString;
use std::io;
use std::rc::Rc;
use rl_sys::readline;

use nix::libc;

mod directive;
use InputType::*;

extern crate parser;
use parser::codemap::CodeMap;
use parser::lexer::{Reader, StringReader};
use parser::parser::{filemap_to_parser, parse_tts_from_source_str, ParseSess, Parser};

static DEFAULT_PROMPT: &'static str = "\x1b[33mflang> \x1b[0m";

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
  sout: Rc<RefCell<io::Write>>, // stream out,
  serr: Rc<RefCell<io::Write>>, // stream err,
}

impl Repl {
  pub fn new(sout: Rc<RefCell<io::Write>>, serr: Rc<RefCell<io::Write>>) -> Repl {
    Repl {
      src_file: SourceFile::new(),
      sout: sout,
      serr: serr,
    }
  }

  pub fn write_out(&self, msg: &[u8]) {
    self.sout.borrow_mut().write(msg).ok();
  }

  pub fn flush_out(&self) {
    self.sout.borrow_mut().flush().ok();
  }

  pub fn write_err(&self, msg: &[u8]) {
    self.sout.borrow_mut().write(msg).ok();
  }

  pub fn flush_err(&self) {
    self.sout.borrow_mut().flush().ok();
  }

  pub fn run(&mut self) {
    self.write_out(b"Welcome to Flang version 0.1. (Type :help for assistance.)\n");
    self.flush_out();

    let mut state = ReplState::new();

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
          self.write_err(format!("ERROR: {}", msg).as_bytes());
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
        x if x.starts_with(':') => Directive(&first[1..], tokens.collect::<Vec<_>>()),
        x if x.starts_with('!') => ExternalComamnd(&line[1..]),
        _ => ExecuteSource(line)
      },
      None => Empty
    }
  }

  pub fn exec_directive(&self, directive: &str, args: Vec<&str>) -> ReplAction {
    match directive {
      "dump" => {
        self.write_out(self.src_file.as_bytes());
        ReplAction::Done
      }
      "quit" => ReplAction::Quit,
      _ => ReplAction::Done
    }
  }

  pub fn exec_line(&mut self, line: &str) -> ReplAction {
    debug!("input line: {}", line);
    let sess: ParseSess = ParseSess::new();
    let filemap = sess.codemap().new_filemap_and_lines("console.fl", None, line);
    let parser = filemap_to_parser(&sess, filemap);

    self.src_file.add_line(line);
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

pub struct ReplState;

impl ReplState {
  pub fn new() -> ReplState {
    ReplState
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


