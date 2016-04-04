use config;

use syntax::ast;
use syntax::errors;
use syntax::errors::emitter::{Emitter, BasicEmitter, EmitterWriter};
use syntax::diagnostics;
use syntax::parse::ParseSess;

use std::cell::{Cell, RefCell};
use std::path::PathBuf;

// Represents the data associated with a compilation
// session for a single crate.
pub struct Session {
  pub opts: config::Options,
  pub parse_sess: ParseSess,
  pub default_sysroot: Option<PathBuf>,
  // The name of the root source file of the crate, in the local file system.
  // The path is always expected to be absolute. `None` means that there is no
  // source file.
  pub local_crate_source_file: Option<PathBuf>,
  pub working_dir: PathBuf,
  pub plugin_llvm_passes: RefCell<Vec<String>>,
  /// The maximum recursion limit for potentially infinitely recursive
  /// operations such as auto-dereference and monomorphization.
  pub recursion_limit: Cell<usize>,

  next_node_id: Cell<ast::NodeId>,
}

impl Session {
  pub fn err(&self, msg: &str) {
    self.diagnostic().err(msg)
  }

  pub fn err_count(&self) -> usize {
    self.diagnostic().err_count()
  }

  pub fn has_errors(&self) -> bool {
    self.diagnostic().has_errors()
  }

  pub fn abort_if_errors(&self) {
    self.diagnostic().abort_if_errors();
  }

  pub fn fatal(&self, msg: &str) -> ! {
    panic!(self.diagnostic().fatal(msg))
  }

  pub fn diagnostic<'a>(&'a self) -> &'a errors::Handler {
    &self.parse_sess.span_diagnostic
  }
}

/*
pub fn build_session(sopts: config::Options,
                     local_crate_source_file: Option<PathBuf>,
                     registry: diagnostics::registry::Registry,
                     cstore: Rc<for<'a> CrateStore<'a>>)
                     -> Session {
}
*/

pub fn early_error(output: config::ErrorOutputType, msg: &str) -> ! {
    let mut emitter: Box<Emitter> = match output {
        config::ErrorOutputType::HumanReadable(color_config) => {
            Box::new(BasicEmitter::stderr(color_config))
        }
        config::ErrorOutputType::Json => panic!("JsonEmitter is not supported yet")
    };
    emitter.emit(None, msg, None, errors::Level::Fatal);
    panic!(errors::FatalError);
}

pub fn early_warn(output: config::ErrorOutputType, msg: &str) {
    let mut emitter: Box<Emitter> = match output {
        config::ErrorOutputType::HumanReadable(color_config) => {
            Box::new(BasicEmitter::stderr(color_config))
        }
        config::ErrorOutputType::Json => panic!("JsonEmitter is not supported yet")
    };
    emitter.emit(None, msg, None, errors::Level::Warning);
}

// Err(0) means compilation was stopped, but no errors were found.
// This would be better as a dedicated enum, but using try! is so convenient.
pub type CompileResult = Result<(), usize>;

pub fn compile_result_from_err_count(err_count: usize) -> CompileResult {
    if err_count == 0 {
        Ok(())
    } else {
        Err(err_count)
    }
}