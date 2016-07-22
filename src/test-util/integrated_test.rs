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

use std::fmt;
use std::ffi::OsStr;
use std::io;
use std::path::{Path, PathBuf};

use difference::{diff, print_diff};
use getopts::{self, Matches, Options};

use util;

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

pub type DriverResult<T> = Result<T, DriverErr>;

pub struct TestDriver<'a> {
  env: &'a RunEnv,
  matches: Matches,
  in_base_path: PathBuf,
  out_base_path: PathBuf,
  test_sets: Vec<(Box<TestSet<'a>>)>
}

impl<'a> TestDriver<'a> {
  pub fn new(env: &'a RunEnv, opts: Options, sets: Vec<Box<TestSet<'a>>>)
      -> DriverResult<TestDriver<'a>> {

    let matches = opts.parse(&env.args()[1..])?;
    if matches.opt_present("h") {
      return Err(DriverErr::Help);
    }
    Self::check_requred_params(&matches)?;

    let (in_base_path, out_base_path) = Self::check_and_get_dirs(env.cwd(), &matches)?;

    Ok(TestDriver {
      env: env,
      matches: matches,
      in_base_path: in_base_path,
      out_base_path: out_base_path,
      test_sets: sets
    })
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
      println!("{}", p.name());
      if self.matches.opt_present(p.name()) {
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

  fn assert(&self, edit_dist: i32, expected: &str, result: &str) -> DriverResult<()> {
    if edit_dist > 0 {
      println!(" Failed, difference:");
      print_diff(expected, result, "");
      println!("");
    } else {
      println!(" Ok")
    }

    Ok(())
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

  fn run_all(&self, driver: &TestDriver) -> DriverResult<()> {
    for p in list_files(self.in_dir(driver).as_path(), "fl")? {
      self.run_unit(driver, p.as_path())?;
    }
    Ok(())
  }

  fn run_unit(&self, driver: &TestDriver, input: &Path) -> DriverResult<()> {
    let test_name = extract_test_name(input);

    // transform an input into a string generated from an output.
    let result = self.transform(input)?;
    self.save_result(driver, test_name, &result)?;

    let expected = self.expected_result(driver, test_name)?;

    print!("Testing {} ... ", display(input.file_name()));
    let (dist, _) = diff(&result, &expected, "");

    self.assert(dist, &expected, &result)
  }

  fn save_result(&self, driver: &TestDriver, test_name: &str, result: &str) -> DriverResult<()> {
    let mut save_path = self.out_dir(driver);
    save_path.push(format!("{}.{}", test_name, "result"));
    Ok(util::str_to_file(save_path.as_path(), result)?)

  }

  fn expected_result(&self, driver: &TestDriver, test_name: &str) -> DriverResult<String> {
    let mut expected_path = self.in_dir(driver);
    expected_path.push(format!("{}.{}", test_name, "result"));
    Ok(util::file_to_string(expected_path.as_path())?)
  }
}