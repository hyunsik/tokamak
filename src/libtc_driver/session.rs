use syntax::errors;
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