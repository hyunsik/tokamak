use std::cell::RefCell;
use std::io::{self, Write};
use std::rc::Rc;
use std::str;
use std::sync::{Arc, Mutex};
use std::thread;

use common::driver::ErrorDestination;
use errors::{self, DiagnosticBuilder, Handler};
use errors::emitter::{ColorConfig, Emitter, EmitterWriter};
use parser::ast::Stmt;
use parser::ast::StmtKind::*;
use parser::codemap::CodeMap;
use parser::lexer::{Reader, StringReader};
use parser::parser::{filemap_to_parser, parse_tts_from_source_str, ParseSess, Parser, PResult};
use parser::tokenstream::TokenTree;

use self::ErrorKind::*;

pub enum IncrCompilerAction {
  Done,
  Error,
  Continue,
  Quit
}

enum ErrorKind {
  UnclosedDelimiter,
  Unknown
}

unsafe impl Send for IncrCompilerAction {}

pub struct IncrCompiler {
  pub filename: String,
  pub errdst: Rc<RefCell<ErrorDestination>>, // error destination
}

impl IncrCompiler {
  pub fn new(filename: String, errdst: Rc<RefCell<ErrorDestination>>)
      -> IncrCompiler {

    IncrCompiler {
      filename: filename,
      errdst: errdst
    }
  }

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
    let line = line.to_owned();
    let filename = self.filename.clone();

    let task = thread::Builder::new().name("parsing".to_owned());
    let handle = task.spawn(move || {

      io::set_panic(Some(Box::new(err)));

      let cm = Rc::new(CodeMap::new());
      let (emitter, errs) = new_emitter(errdst, cm.clone());
      let handler = Handler::with_emitter(true, false, emitter);
      let parsess = ParseSess::with_span_handler(handler, cm.clone());
      let filemap = parsess.codemap().new_filemap_and_lines(&filename, None, &line);
      let mut parser = filemap_to_parser(&parsess, filemap);

      if need_more_liens(&errs) {
        return IncrCompilerAction::Continue;
      } else if is_error(&errs) {
        return IncrCompilerAction::Error;
      }

      IncrCompilerAction::Done
    }).unwrap();

    let action = match handle.join() {
      Ok(result) => result,
      Err(value) => {
        if !value.is::<errors::FatalError>() {
          writeln!(io::stderr(), "{}", str::from_utf8(&data.lock().unwrap()).unwrap()).unwrap();
        }
        IncrCompilerAction::Error
      }
    };

    action
  }
}

fn need_more_liens(errs: &Rc<RefCell<Vec<ErrorKind>>>) -> bool {
  errs.borrow().len() == 1 && match errs.borrow()[0] {
    UnclosedDelimiter => true,
    _ => false
  }
}

fn is_error(errs: &Rc<RefCell<Vec<ErrorKind>>>) -> bool {
  if errs.borrow().len() == 0 {
    return false;
  }

  if errs.borrow().len() > 2 {
    return true;
  }

  // only if errors.len() == 1
  match errs.borrow()[0] {
    UnclosedDelimiter => false,
    _ => true
  }
}

fn new_emitter(errdst: ErrorDestination, cm: Rc<CodeMap>)
      -> (Box<Emitter>, Rc<RefCell<Vec<ErrorKind>>>) {
    let ew = match errdst {
      ErrorDestination::Stderr => {
        EmitterWriter::stderr(ColorConfig::Auto, Some(cm))
      }
      ErrorDestination::Raw(ref buf) => {
        let delegator = WriteDelegator {write: buf.clone()};
        EmitterWriter::new(Box::new(delegator), Some(cm))
      }
    };

    let (emitter, errkinds) = EmitterDelegator::new(ew);
    (Box::new(emitter), errkinds)
}

pub struct WriteDelegator {
  write: Rc<RefCell<Box<io::Write + Send>>>
}

unsafe impl Send for WriteDelegator {}

impl io::Write for WriteDelegator {
  fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
    self.write.borrow_mut().write(buf)
  }

  fn flush(&mut self) -> io::Result<()> {
    self.write.borrow_mut().flush()
  }
}

struct EmitterDelegator {
  em: EmitterWriter,
  errors: Rc<RefCell<Vec<ErrorKind>>>
}

impl EmitterDelegator {
  pub fn new(em : EmitterWriter)
      -> (EmitterDelegator, Rc<RefCell<Vec<ErrorKind>>>) {
    let errors = Rc::new(RefCell::new(Vec::new()));
    let delegator = EmitterDelegator { em : em, errors: errors.clone() };
    (delegator, errors)
  }
}

impl Emitter for EmitterDelegator {
  fn emit(&mut self, db: &DiagnosticBuilder) {

    let errkind = error_kind(db.message());

    match errkind {
      UnclosedDelimiter => {},
      _ => self.em.emit(db)
    };

    self.errors.borrow_mut().push(errkind);
  }
}

fn error_kind(msg: &str) -> ErrorKind {
  if is_unclosed_delemiter(msg) {
    ErrorKind::UnclosedDelimiter
  } else {
    Unknown
  }
}

fn is_unclosed_delemiter(msg: &str) -> bool {
  msg == "this file contains an un-closed delimiter"
}