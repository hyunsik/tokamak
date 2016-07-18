use std::fmt;
use std::ffi::OsStr;
use std::fs::{File};
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use difference::diff;

use util::{self, mkdir};

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

pub type AssertFn = Fn(i32, &str, &str, bool) -> TResult<()>;

pub type ExecFn = Fn(&Path) -> TResult<String>;

pub fn test_base(basedir: &Path, tmpdir: &Path,
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
    let expected = util::file_to_string(expected_path.as_path())?;

    print!("Testing {} ... ", display(fpath.file_name()));

    let (dist, _) = diff(&result, &expected, "");
    assert_fn(dist, &expected, &result, stop_if_err)?
  }

  Ok(())
}

pub enum PTestError {
  ParseError(String),
  TestFailure,
  IOError(io::Error)
}

impl From<io::Error> for PTestError {
  fn from(err: io::Error) -> PTestError {
    PTestError::IOError(err)
  }
}

pub type TResult<T> = Result<T, PTestError>;

fn setup_tmpdir(tmpdir: &Path) {
  mkdir(tmpdir);

  let mut succ_result = PathBuf::from(tmpdir);
  succ_result.push("success");
  mkdir(succ_result.as_path());
}

pub type Phase = Box<Fn(&Path, &Path, bool) -> TResult<()>>;