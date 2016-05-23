use std::mem;
use std::result::Result;

use attr::{ThinAttributes};
use ast::{self, Package, Module, Visibility, Item, Expr, ExprKind, UnOp};
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
  AttributesParsed(ThinAttributes),
  AlreadyParsed(P<Expr>),
}

impl From<Option<ThinAttributes>> for LhsExpr {
  fn from(o: Option<ThinAttributes>) -> Self {
    if let Some(attrs) = o {
      LhsExpr::AttributesParsed(attrs)
    } else {
      LhsExpr::NotYetParsed
    }
  }
}

impl From<P<Expr>> for LhsExpr {
  fn from(expr: P<Expr>) -> Self {
    LhsExpr::AlreadyParsed(expr)
  }
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

  /// Is this expression a successfully-parsed statement?
  fn expr_is_complete(&mut self, e: &Expr) -> bool {
    self.restrictions.contains(RESTRICTION_STMT_EXPR) &&
      !expr_requires_semi_to_be_stmt(e)
  }

  /// Parse an expression
  pub fn parse_expr(&mut self) -> PResult<P<Expr>> {
    self.parse_expr_res(Restrictions::empty(), None)
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
  pub fn parse_expr_res(&mut self, r: Restrictions,
                        already_parsed_attrs: Option<ThinAttributes>)
                        -> PResult<P<Expr>> {
    self.with_res(r, |this| this.parse_assoc_expr(already_parsed_attrs))
  }

  /// Parse an associative expression
  ///
  /// This parses an expression accounting for associativity and precedence of the operators in
  /// the expression.
  pub fn parse_assoc_expr(&mut self,
                          already_parsed_attrs: Option<ThinAttributes>)
                          -> PResult<P<Expr>> {
    self.parse_assoc_expr_with(0, already_parsed_attrs.into())
  }

  /// Parse an associative expression with operators of at least `min_prec` precedence
  pub fn parse_assoc_expr_with(&mut self, min_prec: usize, lhs: LhsExpr)
      -> PResult<P<Expr>> {
    let mut lhs: P<Expr> = if let LhsExpr::AlreadyParsed(expr) = lhs {
      expr
    } else {
      let attrs = match lhs {
        LhsExpr::AttributesParsed(attrs) => Some(attrs),
        _ => None,
      };
      if self.token == token::DotDot || self.token == token::DotDotDot {
        return self.parse_prefix_range_expr(attrs);
      } else {
        self.parse_prefix_expr(attrs)?
      }
    };

    unimplemented!()
  }

  /// Parse a prefix-unary-operator expr
  pub fn parse_prefix_expr(&mut self,
                           already_parsed_attrs: Option<ThinAttributes>)
                           -> PResult<P<Expr>> {
    let lo = self.span.lo;
    let hi;

    // Note: when adding new unary operators, don't forget to adjust Token::can_begin_expr()
    let ex = match self.token {
      token::Not => {
        self.bump();
        let e = self.parse_prefix_expr(None).map(|e| e)?;
        hi = self.last_span.hi;
        self.mk_unary(UnOp::Not, e)
      }
      token::BinOp(token::Minus) => {
        self.bump();
        let e = self.parse_prefix_expr(None).map(|e| e)?;
        hi = self.last_span.hi;
        self.mk_unary(UnOp::Neg, e)
      },
      _ => return self.parse_dot_or_call_expr(None)
    };
    Ok(self.mk_expr(lo, hi, ex, None))
  }

  /// Parse prefix-forms of range notation: `..expr`, `..`, `...expr`
  fn parse_prefix_range_expr(&mut self,
                             already_parsed_attrs: Option<ThinAttributes>)
                             -> PResult<P<Expr>> {
    unimplemented!()
  }

  /// parse a.b, a.1 or a(13) or a[4] or just a
  pub fn parse_dot_or_call_expr(&mut self,
                                already_parsed_attrs: Option<ThinAttributes>)
                                -> PResult<P<Expr>> {
    let lo = self.span.lo;
    let b = self.parse_bottom_expr()?;
    self.parse_dot_or_call_expr_with(b, lo, None)
  }

  pub fn parse_dot_or_call_expr_with(&mut self,
                                     e0: P<Expr>,
                                     lo: BytePos,
                                     attrs: ThinAttributes)
                                     -> PResult<P<Expr>> {
    unimplemented!()
  }

  fn parse_dot_or_call_expr_with_(&mut self, e0: P<Expr>, lo: BytePos) -> PResult<P<Expr>> {
    let mut e = e0;
    //let mut hi;

    loop {
      // expr.f
      if self.eat(&token::Dot) {
        continue;
      }
      if self.expr_is_complete(&e) { break; }
      match self.token {
        // expr(...)
        token::OpenDelim(token::Paren) => {

        }
        // expr[...]
        // Could be either an index expression or a slicing expression.
        token::OpenDelim(token::Bracket) => {

        }
        _ => return Ok(e)
      }
    }

    return Ok(e);
  }

  /// At the bottom (top?) of the precedence hierarchy,
  /// parse things like parenthesized exprs,
  /// return, etc.
  ///
  /// NB: This does not parse outer attributes,
  ///     and is private because it only works
  ///     correctly if called from parse_dot_or_call_expr().
  fn parse_bottom_expr(&mut self) -> PResult<P<Expr>> {

    let lo = self.span.lo;
    let mut hi = self.span.hi;

    let ex: ExprKind;

    // Note: when adding new syntax here, don't forget to adjust Token::can_begin_expr().
    match self.token {
      token::OpenDelim(token::Paren) => {

      }
      token::OpenDelim(token::Brace) => {

      }
      token::BinOp(token::Or) |  token::OrOr => {

      }
      token::OpenDelim(token::Bracket) => {

      }
      _ => {
        if self.eat_keyword(keywords::If) {
        }
        if self.eat_keyword(keywords::For) {
        }
        if self.eat_keyword(keywords::While) {
        }
        if self.eat_keyword(keywords::Loop) {
        }
        if self.eat_keyword(keywords::Continue) {
        }
        if self.eat_keyword(keywords::Match) {
        }
        if self.eat_keyword(keywords::Unsafe) {
        }
        if self.eat_keyword(keywords::Return) {
        } else if self.eat_keyword(keywords::Break) {
        } else if self.token.is_keyword(keywords::Let) {
        } else {
        }
      }
    }
    unimplemented!()
  }

  pub fn mk_expr(&mut self, lo: BytePos, hi: BytePos,
                 node: ExprKind, attrs: ThinAttributes) -> P<Expr> {
    P(Expr {
      id: ast::DUMMY_NODE_ID,
      node: node,
      span: mk_span(lo, hi),
      attrs: attrs,
    })
  }

  pub fn mk_unary(&mut self, unop: ast::UnOp, expr: P<Expr>) -> ast::ExprKind {
    ExprKind::Unary(unop, expr)
  }

  pub fn mk_binary(&mut self, binop: ast::BinOp, lhs: P<Expr>, rhs: P<Expr>) -> ast::ExprKind {
    ExprKind::Binary(binop, lhs, rhs)
  }
}

/// Does this expression require a semicolon to be treated
/// as a statement? The negation of this: 'can this expression
/// be used as a statement without a semicolon' -- is used
/// as an early-bail-out in the parser so that, for instance,
///     if true {...} else {...}
///      |x| 5
/// isn't parsed as (if true {...} else {...} | x) | 5
pub fn expr_requires_semi_to_be_stmt(e: &ast::Expr) -> bool {
  match e.node {
    ast::ExprKind::If |
    ast::ExprKind::Match |
    ast::ExprKind::Block |
    ast::ExprKind::While |
    ast::ExprKind::Loop |
    ast::ExprKind::ForLoop => false,
    _ => true,
  }
}

#[cfg(test)]
mod tests {
  use token::{self, str_to_ident};
  use std::rc::Rc;
  use lexer::{Reader, StringReader};
  use super::Parser;

  fn reader(src: &str) -> Box<Reader> {
    Box::new(StringReader::new(Rc::new(src.to_string())))
  }
  #[test]
  fn test_expr() {
    let mut r = reader("a + 2");
    let mut p = Parser::new(r);
    p.parse_expr();
  }
}
