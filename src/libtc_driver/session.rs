use config;

use syntax::errors;
use syntax::errors::emitter::{Emitter, BasicEmitter, EmitterWriter};
use syntax::diagnostics;
use syntax::parse::ParseSess;

// Represents the data associated with a compilation
// session for a single crate.
pub struct Session {
  pub parse_sess: ParseSess,
}

impl Session {
  pub fn fatal(&self, msg: &str) -> ! {
    panic!(self.diagnostic().fatal(msg))
  }

  pub fn diagnostic<'a>(&'a self) -> &'a errors::Handler {
    &self.parse_sess.span_diagnostic
  }
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