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
use std::fs::{File};
use std::io::{self, Write};
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
  test_sets: Vec<(&'static str, TestSet)>
}

impl<'a> TestDriver<'a> {
  pub fn new(env: &'a RunEnv, opts: Options, phases: Vec<(&'static str, TestSet)>)
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
      test_sets: phases
    })
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
      println!("{}", p.0);
      if self.matches.opt_present(p.0) {
        p.1(&self.in_base_path, &self.out_base_path, false)?
      }
    }

    Ok(())
  }
}

pub fn list_files(dir: &Path, extention: &str) -> io::Result<Vec<PathBuf>> {
  Ok(util::list_files(dir)?
       .map(|e| e.ok().unwrap().path())
       .filter(|p| p.extension().unwrap().to_str().unwrap() == extention)
       .collect::<Vec<PathBuf>>())

}

pub fn extract_test_name<'a>(file_name: &'a Path) -> &'a str{
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

pub type AssertFn = Fn(i32, &str, &str, bool) -> DriverResult<()>;

pub type ExecFn = Fn(&Path) -> DriverResult<String>;

pub fn test_base(basedir: &Path, tmpdir: &Path,
             stop_if_err: bool,
             exec_fn: &ExecFn, assert_fn: &AssertFn)
             -> DriverResult<()> {

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
    let expected = util::file_to_string(expected_path.as_path())?;

    print!("Testing {} ... ", display(fpath.file_name()));

    let (dist, _) = diff(&result, &expected, "");
    assert_fn(dist, &expected, &result, stop_if_err)?
  }

  Ok(())
}

//pub fn setup_tmpdir(tmpdir: &Path) {
//  mkdir(tmpdir);
//
//  let mut succ_result = PathBuf::from(tmpdir);
//  succ_result.push("success");
//  mkdir(succ_result.as_path());
//}


pub type TestSet = Box<Fn(&Path, &Path, bool) -> DriverResult<()>>;

pub trait TestSet2<'a> {
  fn driver(&self) -> &'a TestDriver<'a>;

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

  fn in_dir(&self) -> PathBuf {
    let mut in_dir = PathBuf::from(self.driver().in_base_dir());
    in_dir.push(self.name());
    in_dir
  }

  fn out_dir(&self) -> PathBuf {
    let mut in_dir = PathBuf::from(self.driver().out_base_dir());
    in_dir.push(self.name());
    in_dir
  }

  fn run_all(&self) -> DriverResult<()> {
    for p in list_files(self.in_dir().as_path(), "fl")? {
      self.run_unit(p.as_path())?;
    }
    Ok(())
  }

  fn run_unit(&self, input: &Path) -> DriverResult<()> {
    let test_name = extract_test_name(input);

    // transform an input into a string generated from an output.
    let result = self.transform(input)?;
    self.save_result(test_name, &result)?;

    let expected = self.expected_result(test_name)?;

    print!("Testing {} ... ", display(input.file_name()));
    let (dist, _) = diff(&result, &expected, "");

    self.assert(dist, &expected, &result)
  }

  fn save_result(&self, test_name: &str, result: &str) -> DriverResult<()> {
    let mut save_path = self.out_dir();
    save_path.push(format!("{}.{}", test_name, "result"));
    Ok(util::str_to_file(save_path.as_path(), result)?)

  }

  fn expected_result(&self, test_name: &str) -> DriverResult<String> {
    let mut expected_path = self.in_dir();
    expected_path.push(format!("{}.{}", test_name, "result"));
    Ok(util::file_to_string(expected_path.as_path())?)
  }
}