extern crate rl_sys; // libreadline

use std::cell::RefCell;
use std::collections::HashMap;
use std::io;
use std::rc::Rc;

use rl_sys::readline;

mod directive;
use directive::{
  Directive,
  Help
};

pub struct ReplOption {
}

pub struct ReplEnv {
  directives: HashMap<&'static str, Box<Directive>>,

  pub sout: Rc<RefCell<io::Write>>, // stream out,
  pub serr: Rc<RefCell<io::Write>>, // stream err,
}

impl ReplEnv {
  pub fn new(sout: Rc<RefCell<io::Write>>, serr: Rc<RefCell<io::Write>>) -> ReplEnv {
    let mut directives = HashMap::new();
    let help = Box::new(Help) as Box<Directive>;
    directives.insert(help.command(), help);

    ReplEnv {
      sout: sout,
      serr: serr,
      directives: directives
    }
  }

  pub fn sout_write(&self, msg: &[u8]) {
    self.sout.borrow_mut().write(msg).ok();
  }

  pub fn sout_flush(&self) {
    self.sout.borrow_mut().flush().ok();
  }

  pub fn serr_write(&self, msg: &[u8]) {
    self.sout.borrow_mut().write(msg).ok();
  }

  pub fn serr_flush(&self) {
    self.sout.borrow_mut().flush().ok();
  }
}

pub struct ReplState;

impl ReplState {
  pub fn new() -> ReplState {
    ReplState
  }
}

pub struct Input;

pub struct Output;

pub struct CompilerInstance;

pub struct SourceFile {
  source: String
}

impl SourceFile {
  pub fn add_line(&mut self, line: &str) {
    self.source.push_str(line);
  }
}

fn exec_line(src_file: &mut SourceFile, line: &str) {
  // append source
  // generate mir
  // check ast error
  // add source which is valid
  // skip if the source doesn't need to run
  // generate llvm ir and module
  // reorganize llvm module
}

pub enum ReplAction<'a> {
  Directive(&'a str, Vec<&'a str>),
  ExternalComamnd(&'a str, Vec<&'a str>),
  ExecuteSource(&'a str),
  None,
}

fn parse_action<'a>(env: &ReplEnv, line: &'a str) -> ReplAction<'a> {
  let mut tokens = line.split_whitespace();
  let first_token = tokens.next();

  match first_token {
    Some(first) => match first {
      x if x.starts_with(':') => { // directive
        ReplAction::Directive(&first[1..], tokens.collect::<Vec<_>>())
      }
      x if x.starts_with('!') => { // external command
        ReplAction::ExternalComamnd(&first[1..], tokens.collect::<Vec<_>>())
      }
      _ => {
        ReplAction::ExecuteSource(line)
      }
    },
    None => ReplAction::None
  }
}

fn handle_line(env: &ReplEnv, line: &str) {
  match parse_action(env, line) {
    ReplAction::Directive(_, _) => println!("Directive"),
    ReplAction::ExecuteSource(_) => println!("ExecuteSource"),
    ReplAction::ExternalComamnd(_, _) => println!("ExternalCommand"),
    ReplAction::None => {}
  }
}

pub fn run_repl(env: ReplEnv) {

  env.sout_write(b"Welcome to Flang version 0.1. (Type :help for assistance.)\n");
  env.sout_flush();

  let mut state = ReplState::new();
  loop {
    match readline::readline(&format!("\x1b[33mtkm [{}]> \x1b[0m", "flang")) {
      Ok(Some(line)) => {
        handle_line(&env, &line)
      }
      Ok(None) => break, // eof
      Err(msg) => {
        println!("> {}", msg);
      }
    }
  }
}