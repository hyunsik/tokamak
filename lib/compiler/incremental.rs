use std::cell::RefCell;
use std::io::{self, Write};
use std::rc::Rc;
use std::sync::{Arc, Mutex};

use common::driver::ErrorDestination;


pub enum IncrCompilerAction {
  Done,
  Error,
  Continue,
  Quit
}

unsafe impl Send for IncrCompilerAction {}

pub struct IncrCompiler {
  pub errdst: Rc<RefCell<ErrorDestination>>, // error destination
}

impl IncrCompiler {

  pub fn eval(&mut self, line: &str) -> IncrCompilerAction {

    // Temporarily have stack size set to 16MB to deal with nom-using crates failing
    const STACK_SIZE: usize = 16 * 1024 * 1024; // 16MB

    struct Sink(Arc<Mutex<Vec<u8>>>);
    impl Write for Sink {
        fn write(&mut self, data: &[u8]) -> io::Result<usize> {
            Write::write(&mut *self.0.lock().unwrap(), data)
        }
        fn flush(&mut self) -> io::Result<()> {
            Ok(())
        }
    }

    let data = Arc::new(Mutex::new(Vec::new()));
    let err = Sink(data.clone());
    let errdst = self.errdst.borrow().clone();

    IncrCompilerAction::Done
  }
}