use std::result::Result;

use ast::{self, Package, Module, Item, Expr};
use codemap::{self, Span};
use lexer::{self, Reader, StringReader, TokenAndSpan};
use token::{self, Token};

pub struct Parser {
  pub reader: Box<Reader>,

  /// the current token
  pub token: token::Token,
  /// the span of the current token
  pub span: Span,
  /// the span of the prior token
  pub last_span: Span,

  // token buffer
  pub buffer: [TokenAndSpan; 4],
  pub buffer_start: isize,
  pub buffer_end: isize,

  /// stats
  pub tokens_consumed: usize,
}

pub type PResult<T> = Result<T, ()>;
pub type P<T> = Box<T>;

impl Parser {
  pub fn new(mut r: Box<Reader>) -> Parser {
    let tok0 = r.real_token();
    let span = tok0.sp;
    let placeholder = TokenAndSpan {
      tok: Token::Underscore,
      sp: span
    };

    Parser {
      reader: r,
      token: tok0.tok,
      span: span,
      last_span: span,
      buffer: [
        placeholder.clone(),
        placeholder.clone(),
        placeholder.clone(),
        placeholder.clone(),
      ],
      buffer_start: 0,
      buffer_end: 0,
      tokens_consumed: 0
    }
  }

  pub fn parse_package(&self) -> PResult<P<Package>> {
    unimplemented!()
  }

  pub fn parse_module(&mut self) -> PResult<P<Module>> {
    unimplemented!()
  }

  pub fn parse_item(&mut self) -> PResult<P<Item>> {
    unimplemented!()
  }

  pub fn parse_expr(&mut self) -> PResult<P<Expr>> {
    unimplemented!()
  }
}
