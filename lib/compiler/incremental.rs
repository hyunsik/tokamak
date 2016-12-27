use std::cell::RefCell;
use std::io::{self, Write};
use std::rc::Rc;
use std::str;
use std::sync::{Arc, Mutex};
use std::thread;

use common::driver::ErrorDestination;
use errors::{self, DiagnosticBuilder, Handler};
use errors::emitter::{ColorConfig, Emitter, EmitterWriter};
use parser::ast::{self, Stmt};
use parser::ast::StmtKind::*;
use parser::codemap::CodeMap;
use parser::lexer::{Reader, StringReader};
use parser::parser::{filemap_to_parser, parse_tts_from_source_str, ParseSess, Parser, PResult};
use parser::ptr::P;
use parser::tokenstream::TokenTree;

use self::ErrorKind::*;

use super::Input;

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

enum Parsed {
  Item(P<ast::Item>),
  Stmt(ast::Stmt),
  None,
  Error
}

unsafe impl Send for IncrCompilerAction {}

pub struct IncrCompiler {
  pub filename: String,
  pub errdst: Rc<RefCell<ErrorDestination>>, // error destination
}

// Temporarily have stack size set to 16MB to deal with nom-using crates failing
const DEFAULT_STACK_SIZE: usize = 16 * 1024 * 1024; // 16MB

impl IncrCompiler {
  pub fn new(filename: String, errdst: Rc<RefCell<ErrorDestination>>)
      -> IncrCompiler {

    IncrCompiler {
      filename: filename,
      errdst: errdst
    }
  }

  pub fn init(&mut self) {
  }

  pub fn eval(&mut self, line: &str) -> IncrCompilerAction {
    println!("execute line: {}", line);
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

      debug!("filemap_to_parser...");
      if need_more_liens(&errs) {
        return IncrCompilerAction::Continue;
      } else if is_error(&errs) {
        return IncrCompilerAction::Error;
      }

      match parse(&mut parser) {
        Parsed::Item(item) => {
          debug!("item: {:?}", item);
          IncrCompilerAction::Done
        }
        Parsed::Stmt(stmt) => {
          debug!("stmt: {:?}", stmt);
          IncrCompilerAction::Done
        }
        Parsed::None => {
          debug!("none");
          IncrCompilerAction::Done
        }
        Parsed::Error => {
          debug!("error");
          IncrCompilerAction::Error
        }
      }

    }).unwrap();

    let action = match handle.join() {
      Ok(result) => {
        debug!("handle Ok");
        result
      }
      Err(value) => {
        debug!("handle Err");
        if !value.is::<errors::FatalError>() {
          writeln!(io::stderr(), "{}", str::from_utf8(&data.lock().unwrap()).unwrap()).unwrap();
        }
        IncrCompilerAction::Error
      }
    };

    action
  }
} // IncrCompiler

pub type CompilerResult = Result<(), u64>;

fn compiler_input(input: &Input) {
  match phase_1_parse_input(input) {
    Ok(_) => {},
    Err(_) => {}
  };
}

fn phase_1_parse_input(input: &Input) -> PResult<ast::Package> {
  unimplemented!()
}

fn should_parse_stmt(item: &PResult<Option<P<ast::Item>>>) -> bool {
  // if item is error or none
  item.is_err() || item.as_ref().ok().unwrap().is_none()
}

/// it tries to parse a given line.
/// It firstly assumes the line as an item, and tries to parse the line.
/// Otherwise, it regards
fn parse(parser: &mut Parser) -> Parsed {
  let mut parsed_item = parser.parse_item();

  if parsed_item.is_err() {
    let e = parsed_item.as_mut().err().unwrap();
    e.cancel();
  }

  let mut parsed_stmt = if should_parse_stmt(&parsed_item) {
    parser.parse_full_stmt()
  } else {
    Ok(None)
  };

  if parsed_stmt.is_err() {
    let e = parsed_stmt.as_mut().err().unwrap();
    e.cancel();
  }

  // There are 9 cases:
  // 1: (Ok(Some(item)), Ok(Some(stmt))) - how it happens?
  // 2: (Ok(Some(item)), Ok(None))       - item
  // 3: (Ok(Some(item)), Err(e))         - item
  // 4: (Ok(None)), Ok(Some(stmt)))      - stmt
  // 5: (Err(e),    Ok(Some(stmt)))      - stmt
  // 6: (Ok(None),  Ok(None))            - none
  // 7: (Ok(None),  Err(e))              - none
  // 8: (Err(e),  Ok(None))              - none
  // 9: (Err(e),  Err(e))                - error
  match (parsed_item, parsed_stmt) {
    // case 1
    (Ok(Some(item)), Ok(Some(stmt))) => {
      panic!("A line is regarded as an item as well as a statement.")
    }
    // case 2, 3
    (Ok(Some(item)), _) => {
      Parsed::Item(item)
    }
    // case 4, 5
    (_, Ok(Some(stmt))) => {
      Parsed::Stmt(stmt)
    }
    // case 9
    (Err(_), Err(_)) => {
      Parsed::Error
    }
    // case 6, 7, 8
    (_, _) => {
      Parsed::None
    }
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