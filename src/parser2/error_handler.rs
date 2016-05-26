extern crate term;

pub use self::Level::*;

use std::fmt;

use codemap::{MultiSpan, Span};

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
  fn color(self) -> term::color::Color {
    match self {
      Bug | Fatal | PhaseFatal | Error => term::color::BRIGHT_RED,
      Warning => term::color::YELLOW,
      Note => term::color::BRIGHT_GREEN,
      Help => term::color::BRIGHT_CYAN,
      Cancelled => unreachable!(),
    }
  }

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

  pub fn help(&mut self , msg: &str) -> &mut DiagnosticBuilder {
    unimplemented!()
  }
}

pub struct Handler;

impl Handler {
  pub fn span_err<S: Into<MultiSpan>>(&self, sp: S, msg: &str) {
    unimplemented!()
  }

  pub fn span_bug<S: Into<MultiSpan>>(&self, sp: S, msg: &str) -> ! {
    unimplemented!()
  }

  pub fn struct_span_err<S: Into<MultiSpan>>(&self, sp: S, msg: &str)
      -> DiagnosticBuilder {
    unimplemented!()
  }
}