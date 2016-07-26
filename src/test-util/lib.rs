#![feature(question_mark)]

/// Directory Hierarchy For A Test Collection
///
/// ```
/// [base]
///   |- [Set 1]
///         |- test_X.fl
///         |- test_Y.fl
///         |-    ...
///   |- [Set 2]
///   |- [Set ..]
///   ..
///   |- [Set N]
/// ```
///
/// A test collection consists of multiple test sets, each of which contains
/// multiple test units. All test units in the same test set shares the test ways,
/// including how to assert an actual result against an the expected result,
/// how to handle input data. In most cases, an input data is a source code.
/// A file name in a test set is used as a test name.

extern crate diff;
extern crate getopts;
extern crate term;

pub mod util;

use std::fmt;
use std::ffi::OsStr;
use std::io;
use std::path::{Path, PathBuf};

use getopts::{HasArg, Matches, Occur, Options};

pub struct RunEnv {
  program_name: String,
  args: Vec<String>,
  cwd: PathBuf,
  exec_path: PathBuf
}

impl RunEnv {
  pub fn new(args: Vec<String>, cwd: PathBuf, exec_path: PathBuf) -> RunEnv {
    RunEnv {
      program_name: args[0].clone(),
      args: args,
      cwd: cwd,
      exec_path: exec_path
    }
  }

  pub fn program_name(&self) -> &str {
    &self.program_name
  }

  pub fn args(&self) -> &Vec<String> {
    &self.args
  }

  pub fn cwd(&self) -> &Path {
    self.cwd.as_path()
  }

  pub fn exec_path(&self) -> &Path {
    self.exec_path.as_path()
  }
}

pub enum DriverErr {
  Help,
  IncompletedParameters,
  TestFailure,
  OptionParsing(getopts::Fail),
  IoError(io::Error),
  InputDirNotExist(PathBuf)
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

pub fn default_options<'a>(test_sets: &Vec<Box<TestSet<'a>>>) -> Options {
  let mut opts = Options::new();
  opts.optflag("h", "help", "print this help menu");
  opts.optflag("a", "all", "test all sets");
  opts.opt("f", "filter",
           "filter tests by given keywords (multiple words allowed by comma)",
           "e.g.,) -f=abc,def",
           HasArg::Yes, Occur::Optional);

  for test_set in test_sets {
    opts.optflag("", test_set.name(), &format!("test the '{}' set", test_set.name()));
  }

  opts
}

pub type DriverResult<T> = Result<T, DriverErr>;

pub struct TestDriver<'a> {
  env: &'a RunEnv,
  test_all: bool,
  filter: Vec<String>,
  matches: Matches,
  in_base_path: PathBuf,
  out_base_path: PathBuf,
  test_sets: Vec<(Box<TestSet<'a>>)>
}

impl<'a> TestDriver<'a> {
  pub fn new(env: &'a RunEnv, opts: &'a Options, sets: Vec<Box<TestSet<'a>>>)
    -> DriverResult<TestDriver<'a>> {

    let matches = opts.parse(&env.args()[1..])?;
    if matches.opt_present("h") {
      return Err(DriverErr::Help);
    }
    Self::check_requred_params(&matches)?;
    let test_all = matches.opt_present("a");
    let filter = Self::create_filter(&matches);

    let (in_base_path, out_base_path) = Self::check_and_get_dirs(env.cwd(), &matches)?;
    // create the output base dir if not exists
    util::mkdir(&out_base_path)?;

    Ok(TestDriver {
      env: env,
      test_all: test_all,
      filter: filter,
      matches: matches,
      in_base_path: in_base_path,
      out_base_path: out_base_path,
      test_sets: sets
    })
  }

  fn create_filter(matches: &Matches) -> Vec<String> {
    let mut filter: Vec<String> = Vec::new();
    if let Some(f) = matches.opt_str("f") {
      filter.extend_from_slice(&f.split(",").map(|w| w.to_owned()).collect::<Vec<String>>());
    }
    filter
  }

  pub fn add_testset(&mut self, set: Box<TestSet<'a>>) {
    self.test_sets.push(set);
  }

  pub fn env(&self) -> &RunEnv {
    self.env
  }

  pub fn in_base_dir(&self) -> &Path {
    self.in_base_path.as_path()
  }

  pub fn out_base_dir(&self) -> &Path {
    self.out_base_path.as_path()
  }

  fn check_requred_params(matches: &Matches) -> DriverResult<()> {
    if matches.free.len() == 2 {
      Ok(())
    } else {
      Err(DriverErr::IncompletedParameters)
    }
  }

  fn check_and_get_dirs(cwd: &Path, matches: &Matches) -> DriverResult<(PathBuf, PathBuf)> {
    let in_base_path = Self::check_input_path(util::absolute_path(cwd, &matches.free[0]))?;
    Ok((in_base_path, util::absolute_path(cwd, &matches.free[1])))
  }

  fn check_input_path(path: PathBuf) -> DriverResult<PathBuf> {
    if path.exists() {
      Ok(path)
    } else {
      Err(DriverErr::InputDirNotExist(path))
    }
  }

  pub fn run(&self) -> DriverResult<()> {
    for p in self.test_sets.iter() {
      if self.test_all || self.matches.opt_present(p.name()) {
        p.run_all(self)?
      }
    }

    Ok(())
  }
}

fn list_files(dir: &Path, extention: &str) -> io::Result<Vec<PathBuf>> {
  Ok(util::list_files(dir)?
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

pub struct OsStrDisplay<'a> {
  s: Option<&'a OsStr>
}

pub fn display<'a>(s: Option<&'a OsStr>) -> OsStrDisplay {
  OsStrDisplay {s: s}
}

pub trait TestSet<'a> {
  fn name(&self) -> &str;

  fn assert(&self, expected: &str, result: &str) -> DriverResult<()> {
    if expected != result {
      Err(DriverErr::TestFailure)
    } else {
      Ok(())
    }
  }

  fn print_ok(&self) {
    println!(" Ok");
  }

  fn print_fail(&self, expected: &str, result_file: &Path, result: &str) {
    println!(" Failed, difference from {}\n", result_file.to_str().unwrap());
    print_diff(expected, result);
    println!("");
  }

  fn transform(&self, &Path) -> DriverResult<String>;

  fn in_dir(&self, driver: &TestDriver) -> PathBuf {
    let mut in_dir = PathBuf::from(driver.in_base_dir());
    in_dir.push(self.name());
    in_dir
  }

  fn out_dir(&self, driver: &TestDriver) -> PathBuf {
    let mut in_dir = PathBuf::from(driver.out_base_dir());
    in_dir.push(self.name());
    in_dir
  }

  fn is_matched(&self, driver: &TestDriver, p: &Path) -> bool {
    if driver.filter.len() > 0 {
      let test_name = extract_test_name(p);

      for w in &driver.filter { // if a keyword is matched at least one time
        if test_name.contains(w) {
          return true;
        }
      }
      false
    } else {
      true
    }
  }

  fn run_all(&self, driver: &TestDriver) -> DriverResult<()> {
    let files = list_files(self.in_dir(driver).as_path(), "fl")?;

    let total_num = files.len();
    let mut fail_num = 0usize;
    let mut skipped_num = 0usize;

    println!("\nRunning {} tests in [{}] ... ", files.len(), self.name());

    for p in files {
      if self.is_matched(driver, p.as_path()) {
        match self.run_unit(driver, p.as_path()) {
          Ok(_) => {}
          Err(e) => {
            match e {
              DriverErr::TestFailure => fail_num += 1,
              _ => return Err(e)
            }
          }
        }
      } else {
        skipped_num +=1;
      }
    }
    println!("\nResult : ");
    println!("Test Runs: {}, Failures: {}, Errors: {}, Skipped: {}\n",
             total_num, fail_num, 0usize, skipped_num);
    Ok(())
  }

  fn run_unit(&self, driver: &TestDriver, input: &Path) -> DriverResult<()> {
    let test_name = extract_test_name(input);

    // transform an input into a string generated from an output.
    print!("  {} ... ", display(input.file_name()));
    let result = self.transform(input)?;

    let saved_file = self.save_result(driver, test_name, &result)?;
    let expected = self.expected_result(driver, test_name)?;

    match self.assert(&expected, &result) {
      Ok(_) => {
        self.print_ok();
        Ok(())
      }
      Err(e) => {
        self.print_fail(&expected, &saved_file, &result);
        Err(e)
      }
    }
  }

  fn save_result(&self, driver: &TestDriver, test_name: &str, result: &str)
      -> DriverResult<PathBuf> {
    let mut save_path = self.out_dir(driver);
    util::mkdir(save_path.as_path())?;
    save_path.push(format!("{}.{}", test_name, "result"));
    util::str_to_file(save_path.as_path(), result)?;
    Ok(save_path)
  }

  fn expected_result(&self, driver: &TestDriver, test_name: &str) -> DriverResult<String> {
    let mut expected_path = self.in_dir(driver);
    expected_path.push(format!("{}.{}", test_name, "result"));
    Ok(util::file_to_string(expected_path.as_path())?)
  }
}

fn print_diff(left: &str, right: &str) {
  let mut terminal = term::stdout().unwrap();

  for diff in diff::lines(left, right) {

    match diff {
      diff::Result::Left(l) => {
        terminal.fg(term::color::RED).unwrap();
        println!("-{}", l);
      },

      diff::Result::Both(l, _) => println!(" {}", l),

      diff::Result::Right(r)   => {
        terminal.fg(term::color::GREEN).unwrap();
        println!("+{}", r);
      }
    }

    terminal.reset().unwrap();
  }
}