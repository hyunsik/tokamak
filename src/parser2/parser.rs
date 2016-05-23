use std::mem;
use std::result::Result;

use ast::{self, Package, Module, Visibility, Item, Expr};
use codemap::{self, BytePos, mk_span, Span};
use lexer::{Reader, TokenAndSpan};
use ptr::P;
use token::{self, keywords, Token};

bitflags! {
    pub flags Restrictions: u8 {
        const RESTRICTION_STMT_EXPR         = 1 << 0,
        const RESTRICTION_NO_STRUCT_LITERAL = 1 << 1,
        const NO_NONINLINE_MOD  = 1 << 2,
    }
}

pub enum LhsExpr {
  NotYetParsed,
  AlreadyParsed(P<Expr>),
}

pub struct Parser {
  pub reader: Box<Reader>,

  /// the current token
  pub token: token::Token,
  /// the span of the current token
  pub span: Span,
  /// the previous token or None (only stashed sometimes).
  pub last_token: Option<Box<token::Token>>,
  /// the span of the prior token
  pub last_span: Span,

  pub expected_tokens: Vec<TokenType>,
  pub restrictions: Restrictions,

  // token buffer
  pub buffer: [TokenAndSpan; 4],
  pub buffer_start: isize,
  pub buffer_end: isize,

  /// stats
  pub tokens_consumed: usize,
}

#[derive(PartialEq, Eq, Clone)]
pub enum TokenType {
  Token(token::Token),
  Keyword(keywords::Keyword),
  Operator,
}

pub type PResult<T> = Result<T, ()>;

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
      last_token: None,
      last_span: span,
      buffer: [
        placeholder.clone(),
        placeholder.clone(),
        placeholder.clone(),
        placeholder.clone(),
      ],
      expected_tokens: Vec::new(),
      restrictions: Restrictions::empty(),
      buffer_start: 0,
      buffer_end: 0,
      tokens_consumed: 0
    }
  }

  /// Advance the parser by one token
  pub fn bump(&mut self) {
    self.last_span = self.span;
    // Stash token for error recovery (sometimes; clone is not necessarily cheap).
    self.last_token = if self.token.is_ident() ||
    self.token == token::Comma {
      Some(Box::new(self.token.clone()))
    } else {
      None
    };

    let next = if self.buffer_start == self.buffer_end {
      self.reader.real_token()
    } else {
      // Avoid token copies with `replace`.
      let buffer_start = self.buffer_start as usize;
      let next_index = (buffer_start + 1) & 3;
      self.buffer_start = next_index as isize;

      let placeholder = TokenAndSpan {
        tok: token::Underscore,
        sp: self.span,
      };
      mem::replace(&mut self.buffer[buffer_start], placeholder)
    };
    self.span = next.sp;
    self.token = next.tok;
    self.tokens_consumed += 1;
    self.expected_tokens.clear();
  }

  /// Advance the parser by one token and return the bumped token.
  pub fn bump_and_get(&mut self) -> token::Token {
    let old_token = mem::replace(&mut self.token, token::Underscore);
    self.bump();
    old_token
  }

  /// Check if the next token is `tok`, and return `true` if so.
  ///
  /// This method is will automatically add `tok` to `expected_tokens` if `tok` is not
  /// encountered.
  pub fn check(&mut self, tok: &token::Token) -> bool {
    let is_present = self.token == *tok;
    if !is_present { self.expected_tokens.push(TokenType::Token(tok.clone())); }
    is_present
  }

  /// Consume token 'tok' if it exists. Returns true if the given
  /// token was present, false otherwise.
  pub fn eat(&mut self, tok: &token::Token) -> bool {
    let is_present = self.check(tok);
    if is_present { self.bump() }
    is_present
  }

  pub fn check_keyword(&mut self, kw: keywords::Keyword) -> bool {
    self.expected_tokens.push(TokenType::Keyword(kw));
    self.token.is_keyword(kw)
  }

  /// If the next token is the given keyword, eat it and return
  /// true. Otherwise, return false.
  pub fn eat_keyword(&mut self, kw: keywords::Keyword) -> bool {
    if self.check_keyword(kw) {
      self.bump();
      true
    } else {
      false
    }
  }

  pub fn parse_package(&mut self) -> PResult<Package> {
    let lo = self.span.lo;
    Ok(ast::Package {
      module: self.parse_module(&token::Eof, lo)?,
      span: mk_span(lo, self.span.lo),
    })
  }

  pub fn parse_module(&mut self, term: &token::Token, inner_lo: BytePos)
      -> PResult<Module> {
    unimplemented!()
  }

  pub fn parse_item(&mut self) -> PResult<Item> {
    let lo = self.span.lo;
    let visibility = self.parse_visibility()?;

    if self.eat_keyword(keywords::Import) {
      unimplemented!()
    }

    if self.eat_keyword(keywords::Const) {

    }

    if self.eat_keyword(keywords::Static) {

    }

    if self.eat_keyword(keywords::Type) {

    }

    if self.eat_keyword(keywords::Enum) {

    }

    if self.eat_keyword(keywords::Struct) {

    }

    if self.eat_keyword(keywords::Fn) {

    }

    unreachable!()
  }

  pub fn parse_visibility(&mut self) -> PResult<Visibility> {
    if !self.eat_keyword(keywords::Pub) {
      Ok(Visibility::Inherited)
    } else {
      Ok(Visibility::Public)
    }
  }

  pub fn parse_expr(&mut self) -> PResult<P<Expr>> {
    self.parse_expr_res(Restrictions::empty())
  }

  /// Evaluate the closure with restrictions in place.
  ///
  /// After the closure is evaluated, restrictions are reset.
  pub fn with_res<F, T>(&mut self, r: Restrictions, f: F) -> T
    where F: FnOnce(&mut Self) -> T
  {
    let old = self.restrictions;
    self.restrictions = r;
    let r = f(self);
    self.restrictions = old;
    return r;

  }

  /// Parse an expression, subject to the given restrictions
  pub fn parse_expr_res(&mut self, r: Restrictions)
    -> PResult<P<Expr>> {
    self.with_res(r, |this| this.parse_assoc_expr())
  }

  /// Parse an associative expression
  ///
  /// This parses an expression accounting for associativity and precedence of the operators in
  /// the expression.
  pub fn parse_assoc_expr(&mut self) -> PResult<P<Expr>> {
    self.parse_assoc_expr_with(0, LhsExpr::NotYetParsed)
  }

  /// Parse an associative expression with operators of at least `min_prec` precedence
  pub fn parse_assoc_expr_with(&mut self, min_prec: usize, lhs: LhsExpr)
      -> PResult<P<Expr>> {
    unimplemented!()
  }

  pub fn parse_expr_bottom(&mut self) -> PResult<Expr> {
    unimplemented!()
  }
}
