use config;

use middle::cstore::CrateStore;
use syntax::ast;
use syntax::codemap::{self, Span, MultiSpan};
use syntax::errors::{self, DiagnosticBuilder, Handler};
use syntax::errors::emitter::{Emitter, BasicEmitter, EmitterWriter};
use syntax::diagnostics;
use syntax::parse::ParseSess;

use std::cell::{Cell, RefCell};
use std::path::PathBuf;
use std::rc::Rc;

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
  pub fn struct_span_warn<'a, S: Into<MultiSpan>>(&'a self,
                                                    sp: S,
                                                    msg: &str)
                                                    -> DiagnosticBuilder<'a>  {
      self.diagnostic().struct_span_warn(sp, msg)
  }
  pub fn struct_span_warn_with_code<'a, S: Into<MultiSpan>>(&'a self,
                                                              sp: S,
                                                              msg: &str,
                                                              code: &str)
                                                              -> DiagnosticBuilder<'a>  {
    self.diagnostic().struct_span_warn_with_code(sp, msg, code)
  }
  pub fn struct_warn<'a>(&'a self, msg: &str) -> DiagnosticBuilder<'a>  {
    self.diagnostic().struct_warn(msg)
  }

  pub fn fatal(&self, msg: &str) -> ! {
    panic!(self.diagnostic().fatal(msg))
  }

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

  pub fn track_errors<F, T>(&self, f: F) -> Result<T, usize>
        where F: FnOnce() -> T {
    let old_count = self.err_count();
    let result = f();
    let errors = self.err_count() - old_count;
    if errors == 0 {
      Ok(result)
    } else {
      Err(errors)
    }
  }

  pub fn diagnostic<'a>(&'a self) -> &'a errors::Handler {
    &self.parse_sess.span_diagnostic
  }

  pub fn codemap<'a>(&'a self) -> &'a codemap::CodeMap {
        self.parse_sess.codemap()
  }

  pub fn verbose(&self) -> bool { self.opts.debugging_opts.verbose }
}


pub fn build_session(sopts: config::Options,
                     local_crate_source_file: Option<PathBuf>,
                     registry: diagnostics::registry::Registry,
                     cstore: Rc<for<'a> CrateStore<'a>>)
                     -> Session {
  let treat_err_as_bug = sopts.treat_err_as_bug;

  let codemap = Rc::new(codemap::CodeMap::new());
  let emitter: Box<Emitter> = match sopts.error_format {
    config::ErrorOutputType::HumanReadable(color_config) => {
      Box::new(EmitterWriter::stderr(color_config, Some(registry), codemap.clone()))
    }
    config::ErrorOutputType::Json => {
      //Box::new(JsonEmitter::stderr(Some(registry), codemap.clone()))
      unimplemented!()
    }
  };

  let diagnostic_handler =
    errors::Handler::with_emitter(true, treat_err_as_bug, emitter);

  build_session_(sopts, local_crate_source_file, diagnostic_handler, codemap, cstore)
}

pub fn build_session_(sopts: config::Options,
                      local_crate_source_file: Option<PathBuf>,
                      span_diagnostic: errors::Handler,
                      codemap: Rc<codemap::CodeMap>,
                      cstore: Rc<for<'a> CrateStore<'a>>)
                      -> Session {
  unimplemented!()
}


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