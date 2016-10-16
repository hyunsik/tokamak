extern crate term;

pub use self::Level::*;

use std::cell::Cell;
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
pub enum RenderSpan {
  /// A FullSpan renders with both with an initial line for the
  /// message, prefixed by file:linenum, followed by a summary of
  /// the source code covered by the span.
  FullSpan(MultiSpan),

  /// A suggestion renders with both with an initial line for the
  /// message, prefixed by file:linenum, followed by a summary
  /// of hypothetical source code, where each `String` is spliced
  /// into the lines in place of the code covered by each span.
  Suggestion(CodeSuggestion),
}

#[derive(Clone)]
pub struct CodeSuggestion {
  pub msp: MultiSpan,
  pub substitutes: Vec<String>,
}

#[derive(Clone)]
pub struct DiagnosticBuilder<'a> {
  handler: &'a Handler,
  pub level: Level,
  pub message: String,
  pub code: Option<String>,
  pub span: MultiSpan,
  pub children: Vec<SubDiagnostic>,
}

/// For example a note attached to an error.
#[derive(Clone)]
pub struct SubDiagnostic {
  pub level: Level,
  pub message: String,
  pub span: MultiSpan,
  pub render_span: Option<RenderSpan>,
}

impl<'a> DiagnosticBuilder<'a> {

  /// Convenience function for internal use, clients should use one of the
    /// struct_* methods on Handler.
  fn new(handler: &'a Handler,
         level: Level,
         message: &str) -> DiagnosticBuilder<'a> {
    DiagnosticBuilder::new_with_code(handler, level, None, message)
  }

  /// Convenience function for internal use, clients should use one of the
    /// struct_* methods on Handler.
  fn new_with_code(handler: &'a Handler,
                   level: Level,
                   code: Option<String>,
                   message: &str) -> DiagnosticBuilder<'a> {
    DiagnosticBuilder {
      handler: handler,
      level: level,
      message: message.to_owned(),
      code: code,
      span: MultiSpan::new(),
      children: vec![],
    }
  }


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

  pub fn set_span<S: Into<MultiSpan>>(&mut self, sp: S) -> &mut Self {
    self.span = sp.into();
    self
  }

  #[allow(unused_variables)]
  pub fn span_note<S: Into<MultiSpan>>(&mut self,
                                       sp: S,
                                       msg: &str)
                                       -> &mut DiagnosticBuilder {
    unimplemented!()
  }

  #[allow(unused_variables)]
  pub fn help(&mut self , msg: &str) -> &mut DiagnosticBuilder {
    unimplemented!()
  }

  #[allow(unused_variables)]
  pub fn span_help<S: Into<MultiSpan>>(&mut self,
                                       sp: S,
                                       msg: &str)
                                       -> &mut DiagnosticBuilder {
    unimplemented!()
  }
}

pub struct Handler {
  pub err_count: Cell<usize>,
}

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
  pub fn span_bug_no_panic<S: Into<MultiSpan>>(&self, sp: S, msg: &str) {
    unimplemented!()
  }

  pub fn struct_span_warn<'a, S: Into<MultiSpan>>(&'a self,
                                                  sp: S,
                                                  msg: &str)
                                                  -> DiagnosticBuilder<'a> {
    unimplemented!()
  }

  #[allow(unused_variables)]
  pub fn struct_span_err<'a, S: Into<MultiSpan>>(&'a self,
                                                 sp: S,
                                                 msg: &str)
                                                 -> DiagnosticBuilder<'a> {
    self.bump_err_count();
    let mut result = DiagnosticBuilder::new(self, Level::Error, msg);
    result.set_span(sp);
    result
  }

  #[allow(unused_variables)]
  pub fn struct_span_fatal<'a, S: Into<MultiSpan>>(&'a self, sp: S, msg: &str)
      -> DiagnosticBuilder<'a> {
    println!("{}", msg);
    unimplemented!()
  }

  pub fn bug(&self, msg: &str) -> ! {
    unimplemented!()
  }

  pub fn bump_err_count(&self) {
    self.err_count.set(self.err_count.get() + 1);
  }
}