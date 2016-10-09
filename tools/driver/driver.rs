#![feature(question_mark)]

#[macro_use] extern crate log;
extern crate env_logger;
extern crate getopts;

use std::env;
use std::fmt;
use std::io::{self, Write};
use std::path::PathBuf;
use std::process;

use getopts::{HasArg, Options, Occur};

static DEFAULT_PROGRAM_NAME: &'static str = "unnamed";

#[allow(unused)]
fn driver_options() -> Options {
  let mut opts = Options::new();
  opts.optflag("h", "help", "print this help menu");
  opts.optflag("d", "daemon", "run a flang daemon");

  opts.opt("n", "name",
           "Specify a program name",
           "-f \"hello world\"",
           HasArg::Yes, Occur::Optional);

  opts
}

pub enum DriverAction {
  Repl,
  Batch,
  Daemon
}

pub struct DriverEnv {
  program_name: String,
  src_paths: Vec<PathBuf>,
  cwd: PathBuf,
}

fn setup_driver() -> DriverRes<(DriverAction, DriverEnv)> {
  let opts = driver_options();

  let args: Vec<String> = env::args().collect();
  let matches = opts.parse(&args[1..])?; // 0 is the program path
  let src_paths = &matches.free;

  let action = if src_paths.len() > 0 {
    DriverAction::Batch
  } else {
    DriverAction::Repl
  };

  let program_name = match matches.opt_str("name") {
    Some(name) => name,
    None       => DEFAULT_PROGRAM_NAME.to_string()
  };

  let drv_env = DriverEnv {
    program_name: program_name,
    cwd: env::current_dir().unwrap(),
    src_paths: verify_src_paths(src_paths)?
  };

  Ok((action, drv_env))
}

fn verify_src_paths(src_paths: &Vec<String>) -> DriverRes<Vec<PathBuf>> {
  Ok(src_paths.iter().map(|p| PathBuf::from(p)).collect::<Vec<PathBuf>>())
}

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

pub fn main() {
  env_logger::init().unwrap();

  let (action, driver_env) = match setup_driver() {
    Ok(r) => r,
    Err(e) => {
      io::stderr().write_all(format!("{}\n", e).as_bytes()).ok();
      process::exit(-1);
    }
  };

  let exit_code = match action {
    DriverAction::Repl => unimplemented!(),
    DriverAction::Batch => unimplemented!(),
    DriverAction::Daemon => unimplemented!(),
  };

  process::exit(exit_code);
}