#![feature(question_mark)]

extern crate difference;
extern crate getopts;
extern crate test_util;
extern crate parser2;

use std::env;
use std::io::Write;
use std::path::{Path, PathBuf};
use difference::print_diff;
use getopts::{Matches, Options};

use parser2::ast_printer::{self, NoAnn};
use parser2::codemap::CodeMap;
use parser2::lexer::{StringReader};
use parser2::parser::{ParseSess, Parser};

use test_util as test;
use test_util::{Phase, PTestError};

fn print_usage(program: &str, opts: &Options) {
  let brief = format!("Usage: {} TEST_DIR [options]", program);
  print!("{}", opts.usage(&brief));
}

fn test_parser_succ(basedir: &Path, tmpdir: &Path, stop_if_err: bool) -> TResult<()> {

  let succ_fn = |edit_dist: i32, expected: &str, result: &str, stop_if_err: bool|
    -> TResult<()> {

    if edit_dist > 0 {
      println!(" Failed, difference:");
      print_diff(expected, result, "");
      if stop_if_err {
        return Err(PTestError::TestFailure);
      }
      println!("");
    } else {
      println!(" Ok")
    }

    Ok(())
  };

  test::test_base(basedir, tmpdir, stop_if_err, &src_to_pp, &succ_fn)
}

pub type TResult<T> = Result<T, PTestError>;

fn src_to_pp(src_path: &Path) -> TResult<String> {
  assert!(src_path.is_absolute(),
    "A source path must be absolute path, but it is {}.", src_path.to_str().unwrap());

  let sess: ParseSess = ParseSess::new();
  let codemap = CodeMap::new();
  let filemap = codemap.load_file(src_path)?;

  let reader = StringReader::new(&sess.span_diagnostic, filemap);
  let mut parser = Parser::new(&sess, Box::new(reader));
  let package = match parser.parse_package() {
    Ok(p) => p,
    Err(_) => panic!("Error")
  };

  let rdr: Vec<u8> = test::util::file_to_string(src_path).unwrap().into();
  let mut rdr = &*rdr;
  let mut out = Vec::new();

  {
    let writer: &mut Write = &mut out;
    let no_ann = NoAnn;

    ast_printer::print_package(&codemap,
                               &sess.span_diagnostic,
                               &package,
                               "test".to_string(),
                               &mut rdr,
                               Box::new(writer),
                               &no_ann,
                               false)?;
  }

  Ok(String::from_utf8(out).expect("from_utf8 conversion error..."))
}

#[allow(unused_variables)]
fn test_parser_fail(basedir: &Path, tmpdir: &Path, stop_if_err: bool) -> TResult<()> {
  println!("test_parser_fail!");

  Ok(())
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

fn program_name() -> String {
  return env::args().nth(0).unwrap();
}

fn may_print_usage_and_exit(m: &Matches, opts: &Options) {
  let program = program_name();
  if m.opt_present("h") {
    print_usage(&program, opts);
    std::process::exit(0);
  }
}

fn absolute_path(path: &str) -> PathBuf {
  let p = PathBuf::from(path);
  if p.is_absolute() {
    p
  } else {
    let mut cur_dir = PathBuf::from(std::env::current_dir().unwrap());
    cur_dir.push(p);
    cur_dir
  }
}

fn check_args_or_exit(m: &Matches, opts: &Options) -> (PathBuf, PathBuf) {
  if m.free.len() == 2 {

    let dirs = (absolute_path(&m.free[0]), absolute_path(&m.free[1]));
    if !dirs.0.exists() {
      println!("The base directory '{}' does not exists.", dirs.0.display());
      print_usage(&program_name(), opts);
      std::process::exit(1);
    }
    return dirs;
  } else {
    print_usage(&program_name(), opts);
    std::process::exit(1);
  }
}

fn setup_phases() -> Vec<(&'static str, Phase)> {
  vec![
    ("parser-success", Box::new(test_parser_succ)),
    ("parser-fail",    Box::new(test_parser_fail))
  ]
}

fn display_info(basedir: &Path) {
  println!("Running tests on {}...", basedir.display());
}

pub fn main() {
  let args: Vec<String> = env::args().collect();
  let opts = setup_opts();
  let matches = parse_opts(&opts, &args);
  let (basedir, tmpdir) = check_args_or_exit(&matches, &opts);
  let phases = setup_phases();

  test::setup_tmpdir(&tmpdir);
  display_info(basedir.as_path());
  may_print_usage_and_exit(&matches, &opts);
  test::run_phases(basedir.as_path(), tmpdir.as_path(), &matches, &phases).ok().unwrap();

  std::process::exit(0);
}