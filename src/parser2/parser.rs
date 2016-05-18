
use ast;
use codemap::{self, Span};
use lexer::{self, Reader, StringReader};
use token;

pub struct Parser {
  pub reader: Box<Reader>,

  /// the current token
  pub token: token::Token,
  /// the span of the current token
  pub span: Span,
}
