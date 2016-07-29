use std::fs::{self, File, read_dir};
use std::io::{self, ErrorKind, Read, Write};
use std::path::{Path, PathBuf};

pub fn absolute_path(cwd: &Path, path: &str) -> PathBuf {
  let p = PathBuf::from(path);
  if p.is_absolute() {
    p
  } else {
    let mut cur_dir = cwd.to_path_buf();
    cur_dir.push(p);
    cur_dir
  }
}

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
    return Err(io::Error::new(ErrorKind::NotFound,
             format!("{} does not exists", dir.to_str().unwrap())));
  }

  if !fs::metadata(dir)?.is_dir() {
    return Err(io::Error::new(ErrorKind::Other,
             format!("{} must be a directory", dir.to_str().unwrap())));
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

pub fn str_to_file(path: &Path, str: &str) -> io::Result<()> {
  let mut save_file = File::create(&path)?;
  save_file.write_all(str.as_bytes())?;
  save_file.sync_all()?;
  Ok(())
}
