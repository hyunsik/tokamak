extern crate rl_sys; // libreadline

use std::cell::RefCell;
use std::io;
use std::rc::Rc;

use rl_sys::readline;

pub struct ReplOption {
}

pub struct ReplEnv {
  pub sout: Rc<RefCell<io::Write>>, // stream out,
  pub serr: Rc<RefCell<io::Write>>, // stream err,
}

pub struct ReplState;

impl ReplState {
  pub fn new() -> ReplState {
    ReplState
  }
}

pub struct Input;

pub struct Output;

impl ReplEnv {
  pub fn input(&self, line: &str) -> &Input {
    unimplemented!()
  }

  pub fn output(&mut self) -> &mut Output {
    unimplemented!()
  }

  pub fn handle_input(&self, c: &CompilerInstance, input: &Input) -> bool {
    unimplemented!()
  }
}

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

pub enum ReplAction {
  DumpSource,
  ExecuteSource,
  Help(String),
  None,
  Quit,
  ExternalComamnd(Vec<String>),
  UnknownDirective(String),
}

fn parse_action(line: &str) -> ReplAction {
  let mut tokens = line.split_whitespace();
  let first_token = tokens.next();

  match first_token {

    Some(first) => match first {

      x if x.starts_with(':') => { // directive
        let directive = &first[1..];
        match directive {
          "help" => ReplAction::Help("".to_string()),
          "quit" => ReplAction::Quit,
          _ => ReplAction::UnknownDirective(directive.to_string())
        }
      }

      x if x.starts_with('!') => { // external command
        ReplAction::ExternalComamnd(Vec::new())
      }

      _ => {
        ReplAction::ExecuteSource
      }
    },
    None => ReplAction::None
  }
}

fn handle_line(line: &str) {
  match parse_action(line) {
    ReplAction::DumpSource => println!("DumpSource"),
    ReplAction::ExecuteSource => println!("ExecuteSource"),
    ReplAction::ExternalComamnd(_) => println!("ExternalCommand"),
    ReplAction::Help(_) => println!("help"),
    ReplAction::None => println!("none"),
    ReplAction::Quit => println!("quit"),
    ReplAction::UnknownDirective(d) => println!("Unknown directive: {}", d),
  }
}

pub fn run_repl(env: ReplEnv) {

  env.sout.borrow_mut().write_all(b"Welcome to Flang version 0.1. (Type :help for assistance.)\n").ok();
  env.sout.borrow_mut().flush().ok();

  let mut state = ReplState::new();
  loop {
    match readline::readline(&format!("\x1b[33mtkm [{}]> \x1b[0m", "flang")) {
      Ok(Some(line)) => {
        handle_line(&line)
      }
      Ok(None) => break, // eof
      Err(msg) => {
        println!("> {}", msg);
      }
    }
  }
}