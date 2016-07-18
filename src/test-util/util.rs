use std::fs::{self, File, read_dir};
use std::io::{self, Read};
use std::path::Path;

pub enum TestError {
  IOError(io::Error)
}

impl From<io::Error> for TestError {
  fn from(err: io::Error) -> TestError {
    TestError::IOError(err)
  }
}

pub fn mkdir(path: &Path) -> io::Result<()> {
  if !path.exists() {
    fs::create_dir(path)?
  }

  Ok(())
}

pub fn list_files(dir: &Path) -> io::Result<fs::ReadDir> {
  // for each file, it compiles and checks if the file causes error or not.
  //pub fn dir_iter()

  if !dir.exists() {
    panic!("{} does not exists", dir.to_str().unwrap());
  }

  if !fs::metadata(dir)?.is_dir() {
    panic!("{} must be a directory");
  }

  read_dir(dir)
}

pub fn for_each<F>(dir: &Path, f: F) -> Result<(), TestError>
      where F: Fn(&Path) -> Result<(), TestError> {
  // for each file, it compiles and checks if the file causes error or not.
  //pub fn dir_iter()

  if !fs::metadata(dir)?.is_dir() {
    panic!("{} must be a directory");
  }

  for entry in read_dir(dir)? {
    let entry = entry?;
    f(entry.path().as_path())?
  }

  Ok(())
}

pub fn file_to_string(dir: &Path) -> io::Result<String> {
  let mut f = File::open(dir)?;
  let mut buf = String::new();
  f.read_to_string(&mut buf)?;

  Ok(buf)
}
