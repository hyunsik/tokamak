#[macro_use] extern crate log;
extern crate env_logger;
extern crate getopts;
extern crate repl;

extern crate flang_common;

use std::cell::RefCell;
use std::fmt;
use std::io::{self, Write};
use std::path::PathBuf;
use std::rc::Rc;
use std::process;

use getopts::{HasArg, Options, Occur};

pub use repl::{DriverEnv, ErrorDestination};
use repl::{Repl};

static DEFAULT_PROGRAM_NAME: &'static str = "unnamed";

pub enum DriverErr {
  OptionParsing(getopts::Fail),
  SrcFileOrDirDoesntExist(PathBuf),
  IoError(io::Error),
}

impl From<getopts::Fail> for DriverErr {
  fn from(err: getopts::Fail) -> DriverErr {
    DriverErr::OptionParsing(err)
  }
}

impl From<io::Error> for DriverErr {
  fn from(err: io::Error) -> DriverErr {
    DriverErr::IoError(err)
  }
}

impl fmt::Display for DriverErr {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match *self {
      DriverErr::OptionParsing(ref e) => write!(f, "{}", e),
      DriverErr::SrcFileOrDirDoesntExist(ref p) => write!(f, "{} doesn't exist", p.display()),
      DriverErr::IoError(ref e) => write!(f, "{}", e),
    }
  }
}

pub type DriverRes<T> = Result<T, DriverErr>;

pub enum DriverAction {
  Repl,
  Batch,
}

#[allow(unused)]
fn create_options() -> Options {
  let mut opts = Options::new();
  opts.optflag("h", "help", "print this help menu");
  opts.optflag("d", "daemon", "run a flang daemon");

  opts.opt("n", "name",
           "Specify a program name",
           "-f \"hello world\"",
           HasArg::Yes, Occur::Optional);

  opts
}

fn setup_driver(args: Vec<String>, cwd: PathBuf,
               errdst: Rc<RefCell<ErrorDestination>>)
              -> DriverRes<(DriverAction, DriverEnv)> {

  let opts = create_options();
  let parsed_opts = opts.parse(&args[1..])?; // exclude 0'th which is the program path
  let src_paths = &parsed_opts.free;

  let action = if src_paths.len() > 0 {
    DriverAction::Batch
  } else {
    DriverAction::Repl
  };

  let program_name = match parsed_opts.opt_str("name") {
    Some(name) => name,
    None       => DEFAULT_PROGRAM_NAME.to_string()
  };

  let drv_env = DriverEnv {
    program_name: program_name,
    cwd: cwd,
    src_paths: string_to_pathbuf(src_paths),
    errdst: errdst
  };

  Ok((action, drv_env))
}

pub fn run_driver(args: Vec<String>, cwd: PathBuf,
           errdst: ErrorDestination) -> i32 {
  let errdst = Rc::new(RefCell::new(errdst));

  let (action, driver_env) = match setup_driver(args, cwd, errdst.clone()) {
    Ok(r) => r,
    Err(e) => {
      println(&errdst, &format!("{}", e));
      process::exit(-1);
    }
  };

  match action {
    DriverAction::Repl => {
      let mut repl = Repl::new(driver_env);
      repl.run();
      0
    }
    DriverAction::Batch => unimplemented!(),
  }
}

fn string_to_pathbuf(src_paths: &Vec<String>) -> Vec<PathBuf> {
  src_paths.iter().map(|p| PathBuf::from(p)).collect::<Vec<PathBuf>>()
}

fn println(errdst: &Rc<RefCell<ErrorDestination>>, s: &str) {
  errdst.borrow_mut().write_all(s.as_bytes()).ok();
}