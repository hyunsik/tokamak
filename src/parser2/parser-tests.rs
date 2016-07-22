#![feature(question_mark)]

extern crate difference;
extern crate getopts;
extern crate test_util;
extern crate parser2;

use std::env;
use std::io::Write;
use std::path::Path;
use difference::print_diff;
use getopts::Options;

use parser2::ast_printer::{self, NoAnn};
use parser2::codemap::CodeMap;
use parser2::lexer::{StringReader};
use parser2::parser::{ParseSess, Parser};

use test_util as test;
use test_util::{
  DriverErr,
  DriverResult,
  RunEnv,
  TestDriver,
  TestSet,
  TestSet2
};

fn print_usage(program: &str, opts: &Options) {
  let brief = format!("Usage: {} TEST_DIR [options]", program);
  print!("{}", opts.usage(&brief));
}

fn test_parser_succ(basedir: &Path, tmpdir: &Path, stop_if_err: bool) -> DriverResult<()> {

  let succ_fn = |edit_dist: i32, expected: &str, result: &str, stop_if_err: bool|
    -> DriverResult<()> {

    if edit_dist > 0 {
      println!(" Failed, difference:");
      print_diff(expected, result, "");
      if stop_if_err {
        return Err(DriverErr::TestFailure);
      }
      println!("");
    } else {
      println!(" Ok")
    }

    Ok(())
  };

  test::test_base(basedir, tmpdir, stop_if_err, &src_to_pp, &succ_fn)
}

fn src_to_pp(src_path: &Path) -> DriverResult<String> {
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
fn test_parser_fail(basedir: &Path, tmpdir: &Path, stop_if_err: bool) -> DriverResult<()> {
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

fn setup_phases() -> Vec<(&'static str, TestSet)> {
  vec![
    ("parser-success", Box::new(test_parser_succ)),
    ("parser-fail",    Box::new(test_parser_fail))
  ]
}

fn setup_phases2<'a>(driver: &'a TestDriver) -> Vec<Box<TestSet2<'a> + 'a>> {
  vec![
    Box::new(ParserTestSet::new(driver, "success"))
  ]
}

pub fn main() {
  let run_env = RunEnv::new(
    env::args().collect(),
    env::current_dir().unwrap(),
    env::current_exe().unwrap()
  );

  let driver = match TestDriver::new(&run_env, setup_opts(), setup_phases()) {
    Ok(d) => d,
    Err(DriverErr::Help) | Err(DriverErr::IncompletedParameters) => {
      print_usage(run_env.program_name(), &setup_opts());
      std::process::exit(-1);
    },
    Err(DriverErr::OptionParsing(e)) => {
      println!("ERROR: {}\n", e);
      print_usage(run_env.program_name(), &setup_opts());
      std::process::exit(-1);
    }
    Err(DriverErr::InputDirNotExist(p)) => {
      println!("ERROR: {} does not exists...\n", p.to_str().unwrap());
      print_usage(run_env.program_name(), &setup_opts());
      std::process::exit(-1);
    }
    _ => {
      panic!("Error");
    }
  };

  match driver.run() {
    Ok(_) => std::process::exit(0),
    _     => std::process::exit(-1)
  };
}

pub struct ParserTestSet<'a> {
  driver: &'a TestDriver<'a>,
  name: String
}

impl<'a> ParserTestSet<'a> {
  pub fn new(driver: &'a TestDriver, name: &str) -> ParserTestSet<'a> {
    ParserTestSet {
      driver: driver,
      name: name.to_owned()
    }
  }
}

impl<'a> TestSet2<'a> for ParserTestSet<'a> {
  fn driver(&self) -> &'a TestDriver<'a> {
    self.driver
  }

  fn name(&self) -> &str {
    &self.name
  }

  fn transform(&self, path: &Path) -> DriverResult<String> {
    assert!(path.is_absolute(),
    "A source path must be absolute path, but it is {}.", path.to_str().unwrap());

    let sess: ParseSess = ParseSess::new();
    let codemap = CodeMap::new();
    let filemap = codemap.load_file(path)?;

    let reader = StringReader::new(&sess.span_diagnostic, filemap);
    let mut parser = Parser::new(&sess, Box::new(reader));
    let package = match parser.parse_package() {
      Ok(p) => p,
      Err(_) => panic!("Error")
    };

    let rdr: Vec<u8> = test::util::file_to_string(path).unwrap().into();
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
}