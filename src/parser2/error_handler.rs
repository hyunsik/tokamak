extern crate term;

pub use self::Level::*;

use std::fmt;

use codemap::{MultiSpan};

#[derive(Copy, PartialEq, Clone, Debug)]
pub enum Level {
  Bug,
  Fatal,
  // An error which while not immediately fatal, should stop the compiler
  // progressing beyond the current phase.
  PhaseFatal,
  Error,
  Warning,
  Note,
  Help,
  Cancelled,
}

impl fmt::Display for Level {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    self.to_str().fmt(f)
  }
}

impl Level {
  #[allow(dead_code)]
  fn color(self) -> term::color::Color {
    match self {
      Bug | Fatal | PhaseFatal | Error => term::color::BRIGHT_RED,
      Warning => term::color::YELLOW,
      Note => term::color::BRIGHT_GREEN,
      Help => term::color::BRIGHT_CYAN,
      Cancelled => unreachable!(),
    }
  }

  #[allow(dead_code)]
  fn to_str(self) -> &'static str {
    match self {
      Bug => "error: internal compiler error",
      Fatal | PhaseFatal | Error => "error",
      Warning => "warning",
      Note => "note",
      Help => "help",
      Cancelled => panic!("Shouldn't call on cancelled error"),
    }
  }
}

#[derive(Clone)]
pub struct DiagnosticBuilder;

impl DiagnosticBuilder {
  /// Emit the diagnostic.
  pub fn emit(&mut self) {
    unimplemented!()
  }

  /// Cancel the diagnostic (a structured diagnostic must either be emitted or
  /// cancelled or it will panic when dropped).
  /// BEWARE: if this DiagnosticBuilder is an error, then creating it will
  /// bump the error count on the Handler and cancelling it won't undo that.
  /// If you want to decrement the error count you should use `Handler::cancel`.
  pub fn cancel(&mut self) {
    unimplemented!()
  }

  #[allow(unused_variables)]
  pub fn note(&mut self, msg: &str) -> &mut DiagnosticBuilder {
    unimplemented!()
  }

  #[allow(unused_variables)]
  pub fn help(&mut self , msg: &str) -> &mut DiagnosticBuilder {
    unimplemented!()
  }
}

pub struct Handler;

impl Handler {
  #[allow(unused_variables)]
  pub fn cancel(&mut self, err: &mut DiagnosticBuilder) {
    unimplemented!()
  }

  #[allow(unused_variables)]
  pub fn span_err<S: Into<MultiSpan>>(&self, sp: S, msg: &str) {
    unimplemented!()
  }

  #[allow(unused_variables)]
  pub fn span_bug<S: Into<MultiSpan>>(&self, sp: S, msg: &str) -> ! {
    unimplemented!()
  }

  #[allow(unused_variables)]
  pub fn struct_span_err<S: Into<MultiSpan>>(&self, sp: S, msg: &str)
      -> DiagnosticBuilder {
    unimplemented!()
  }

  #[allow(unused_variables)]
  pub fn struct_span_fatal<S: Into<MultiSpan>>(&self, sp: S, msg: &str)
      -> DiagnosticBuilder {
    unimplemented!()
  }
}