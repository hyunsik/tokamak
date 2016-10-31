use std::cell::RefCell;
use std::io;
use std::rc::Rc;
use std::path::PathBuf;

use self::ErrorDestination::*;

#[derive(Clone)]
pub enum ErrorDestination {
  Stderr,
  Raw(Rc<RefCell<Box<io::Write + Send>>>)
}

unsafe impl Send for ErrorDestination {}

impl io::Write for ErrorDestination {
  fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
    match *self {
      Stderr => io::stderr().write(buf),
      Raw(ref w) => w.borrow_mut().write(buf)
    }
  }

  fn flush(&mut self) -> io::Result<()> {
    match *self {
      Stderr => io::stderr().flush(),
      Raw(ref w) => w.borrow_mut().flush()
    }
  }
}

pub struct DriverEnv {
  pub program_name: String,
  pub src_paths: Vec<PathBuf>,
  pub cwd: PathBuf,
  pub errdst: Rc<RefCell<ErrorDestination>>, // stream err,
}