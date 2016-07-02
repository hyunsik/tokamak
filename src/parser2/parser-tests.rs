#![feature(question_mark)]

extern crate difference;
extern crate getopts;
extern crate test_util;
extern crate parser2;

use std::env;
use std::fmt;
use std::ffi::OsStr;
use std::fs::{self, File};
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use difference::{diff, print_diff};
use getopts::{Matches, Options};

use parser2::ast_printer::{self, NoAnn};
use parser2::codemap::CodeMap;
use parser2::error_handler::{DiagnosticBuilder};
use parser2::lexer::{StringReader};
use parser2::parser::{ParseSess, Parser};

use test_util::file as test;

fn print_usage(program: &str, opts: &Options) {
  let brief = format!("Usage: {} TEST_DIR [options]", program);
  print!("{}", opts.usage(&brief));
}

fn list_files(dir: &Path, extention: &str) -> io::Result<Vec<PathBuf>> {
  Ok(test::list_files(dir)?
    .map(|e| e.ok().unwrap().path())
    .filter(|p| p.extension().unwrap().to_str().unwrap() == extention)
    .collect::<Vec<PathBuf>>())

}

fn extract_test_name<'a>(file_name: &'a Path) -> &'a str{
  file_name.file_stem().unwrap().to_str().unwrap()
}

impl<'a> fmt::Display for OsStrDisplay<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.s.unwrap().to_str().unwrap())
  }
}

struct OsStrDisplay<'a> {
  s: Option<&'a OsStr>
}

fn display<'a>(s: Option<&'a OsStr>) -> OsStrDisplay {
  OsStrDisplay {s: s}
}

pub type AssertFn = Fn(i32, &str, &str, bool) -> TResult<()>;

pub type ExecFn = Fn(&Path) -> TResult<String>;

fn test_base(basedir: &Path, tmpdir: &Path,
             stop_if_err: bool,
             exec_fn: &ExecFn, assert_fn: &AssertFn)
             -> TResult<()> {

  let mut succ_dir = PathBuf::from(basedir);
  succ_dir.push("success");
  let succ_dir = succ_dir.as_path();

  let mut succ_res_dir = PathBuf::from(tmpdir);
  succ_res_dir.push("success");
  let succ_res_dir = succ_res_dir.as_path();

  for p in list_files(succ_dir, "fl")? {
    let fpath = p.as_path();
    let test_name = extract_test_name(fpath);

    // convert source into ast, and then generates a source code from ast.
    let result = exec_fn(fpath)?;

    let mut res_path = PathBuf::from(succ_res_dir);
    res_path.push(format!("{}.{}", test_name, "result"));
    let mut result_file = File::create(&res_path)?;
    result_file.write_all(result.as_bytes())?;
    result_file.sync_all()?;

    let mut expected_path = PathBuf::from(succ_dir);
    expected_path.push(format!("{}.{}", test_name, "result"));
    let expected = test::file_to_string(expected_path.as_path())?;

    print!("Testing {} ... ", display(fpath.file_name()));

    let (dist, _) = diff(&result, &expected, "");
    assert_fn(dist, &expected, &result, stop_if_err)?
  }

  Ok(())
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

  test_base(basedir, tmpdir, stop_if_err, &src_to_pp, &succ_fn)
}

pub enum PTestError {
  ParseError(DiagnosticBuilder),
  TestFailure,
  IOError(io::Error)
}

impl From<DiagnosticBuilder> for PTestError {
  fn from(err: DiagnosticBuilder) -> PTestError {
    PTestError::ParseError(err)
  }
}

impl From<io::Error> for PTestError {
  fn from(err: io::Error) -> PTestError {
    PTestError::IOError(err)
  }
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
  let package = parser.parse_package()?;

  let rdr: Vec<u8> = test::file_to_string(src_path).unwrap().into();
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

fn setup_tmpdir(tmpdir: &Path) {
  mkdir(tmpdir);

  let mut succ_result = PathBuf::from(tmpdir);
  succ_result.push("success");
  mkdir(succ_result.as_path());
}

fn mkdir(path: &Path) {
  if !path.exists() {
    match fs::create_dir(path) {
      Ok(_) => {},
      Err(why) => {
        println!("! {:?}", why.kind());
        std::process::exit(1);
      }
    }
  }
}

pub type Phase = Box<Fn(&Path, &Path, bool) -> TResult<()>>;

fn setup_phases() -> Vec<(&'static str, Phase)> {
  vec![
    ("parser-success", Box::new(test_parser_succ)),
    ("parser-fail",    Box::new(test_parser_fail))
  ]
}

fn run_phases(data_dir: &Path, tmp_dir: &Path,
              m: &Matches,
              phases: &Vec<(&'static str, Phase)>)
              -> TResult<()> {

  for p in phases.iter() {
    if m.opt_present(p.0) {
      p.1(data_dir, tmp_dir, false)?
    }
  }

  Ok(())
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

  setup_tmpdir(&tmpdir);
  display_info(basedir.as_path());
  may_print_usage_and_exit(&matches, &opts);
  run_phases(basedir.as_path(), tmpdir.as_path(), &matches, &phases).ok().unwrap();

  std::process::exit(0);
}