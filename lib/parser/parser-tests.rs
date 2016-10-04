#![feature(question_mark)]

#[macro_use] extern crate log;
extern crate env_logger;
extern crate getopts;
extern crate test_util;
extern crate parser2;

use std::env;
use std::io::Write;
use std::path::Path;
use getopts::Options;

use parser2::ast_printer::{self, NoAnn};
use parser2::codemap::CodeMap;
use parser2::lexer::{StringReader};
use parser2::parser::{ParseSess, Parser};

use test_util as test;
use test_util::{
  DriverErr,
  DriverResult,
  default_options,
  RunEnv,
  TestDriver,
  TestSet
};

pub struct ParserTestSet {
  name: String
}

impl ParserTestSet {
  pub fn new(name: &str) -> ParserTestSet {
    ParserTestSet {
      name: name.to_owned()
    }
  }
}

impl<'a> TestSet<'a> for ParserTestSet {

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

fn print_usage(program: &str, opts: &Options) {
  let brief = format!("Usage: {} TEST_DIR [options]", program);
  print!("{}", opts.usage(&brief));
}

fn setup_phases<'a>() -> Vec<Box<TestSet<'a>>> {
  vec![
    Box::new(ParserTestSet::new("items")),
    Box::new(ParserTestSet::new("exprs")),
    Box::new(ParserTestSet::new("literals")),
  ]
}

pub fn main() {
  env_logger::init().unwrap();

  let run_env = RunEnv::new(
    env::args().collect(),
    env::current_dir().unwrap(),
    env::current_exe().unwrap()
  );

  let opts = default_options(&setup_phases());

  let driver = match TestDriver::new(&run_env, &opts, setup_phases()) {
    Ok(d) => d,
    Err(DriverErr::Help) | Err(DriverErr::IncompletedParameters) => {
      print_usage(run_env.program_name(), &opts);
      std::process::exit(-1);
    },
    Err(DriverErr::OptionParsing(e)) => {
      println!("ERROR: {}\n", e);
      print_usage(run_env.program_name(), &opts);
      std::process::exit(-1);
    }
    Err(DriverErr::InputDirNotExist(p)) => {
      println!("ERROR: {} does not exists...\n", p.to_str().unwrap());
      print_usage(run_env.program_name(), &opts);
      std::process::exit(-1);
    }
    Err(DriverErr::IoError(e)) => {
      println!("IO Error: {} \n", e);
      std::process::exit(-1);
    }
    _ => {
      panic!("Error");
    }
  };

  match driver.run() {
    Ok(_) => std::process::exit(0),
    Err(DriverErr::IoError(e)) => {
      println!("\nIO Error: {} \n", e);
      std::process::exit(-1);
    }
    _ => {
      println!("\nInternal Error! \n");
      std::process::exit(-1);
    }
  };
}