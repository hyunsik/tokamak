extern crate getopts;
extern crate test_util;

use std::env;
use std::path::{Path, PathBuf};
use getopts::{Matches, Options};

fn print_usage(program: &str, opts: &Options) {
  let brief = format!("Usage: {} TEST_DIR [options]", program);
  print!("{}", opts.usage(&brief));
}

fn test_parser_succ() {
  // for each file in dir
  //   parser source file into AST
  //   transform AST to source via pretty printer
  //   read a expected result from a file
  //   compare a pretty printed result with a expected result
  // end for
  println!("test_parser_success!")
}

fn test_parser_fail() {
  println!("test_parser_fail!")
}

fn setup_opts() -> Options {
  let mut opts = Options::new();
  opts.optflag("h", "help", "print this help menu")
      .optflag("", "parser-success", "Test parsing success")
      .optflag("", "parser-fail", "Test parsing failures");

  opts
}

fn parse_opts(opts: &Options, args: &Vec<String>) -> Matches {
  match opts.parse(&args[1..]) {
    Ok(m) => m,
    Err(e) => {
      println!("{}", e);
      std::process::exit(1);
    }
  }
}

fn get_program_name() -> String {
  return env::args().nth(0).unwrap();
}

fn may_print_usage_and_exit(m: &Matches, opts: &Options) {
  let program = get_program_name();
  if m.opt_present("h") {
    print_usage(&program, opts);
    std::process::exit(0);
  }
}

fn ensure_basedir_or_exit(m: &Matches, opts: &Options) -> PathBuf {
  if m.free.len() > 0 {
    let p = PathBuf::from(&m.free[0]);

    if p.is_absolute() {
      p
    } else {
      let mut cur_dir = PathBuf::from(std::env::current_dir().unwrap());
      cur_dir.push(p);
      cur_dir
    }
  } else {
    print_usage(&get_program_name(), opts);
    std::process::exit(1);
  }
}

fn setup_phases() -> Vec<(&'static str, Box<Fn()>)> {
  vec![
    ("parser-success", Box::new(test_parser_succ)),
    ("parser-fail",    Box::new(test_parser_fail))
  ]
}

fn run_phases(path: &Path, m: &Matches, phases: &Vec<(&'static str, Box<Fn()>)>) {
  for p in phases.iter() {
    if m.opt_present(p.0) {
      p.1()
    }
  }
}

fn display_info(basedir: &Path) {
  println!("Running tests on {}", basedir.to_str().unwrap());
}

pub fn main() {
  let args: Vec<String> = env::args().collect();
  let opts = setup_opts();
  let matches = parse_opts(&opts, &args);
  let basedir = ensure_basedir_or_exit(&matches, &opts);
  let phases = setup_phases();

  display_info(basedir.as_path());
  may_print_usage_and_exit(&matches, &opts);
  run_phases(&Path::new(&basedir), &matches, &phases);

  std::process::exit(0);
}