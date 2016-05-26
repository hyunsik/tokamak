use std::iter;
use std::mem;
use std::str;
use std::rc::Rc;
use std::result::Result;

use attr::{ThinAttributes};
use ast::{self, Expr, ExprKind, Lit, LitKind, Module, Item, Package, UnOp, Visibility};
use codemap::{self, BytePos, mk_span, Span};
use error_handler::{DiagnosticBuilder, Handler};
use lexer::{char_at, Reader, TokenAndSpan};
use ptr::P;
use token::{self, keywords, InternedString, Token};

bitflags! {
    pub flags Restrictions: u8 {
        const RESTRICTION_STMT_EXPR         = 1 << 0,
        const RESTRICTION_NO_STRUCT_LITERAL = 1 << 1,
        const NO_NONINLINE_MOD  = 1 << 2,
    }
}

/// Info about a parsing session.
pub struct ParseSess {
  pub span_diagnostic: Handler, // better be the same as the one in the reader!
}

impl ParseSess {
  pub fn new() -> ParseSess {
    ParseSess {
      span_diagnostic: Handler
    }
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

pub struct Parser<'a> {
  pub sess: &'a ParseSess,
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

impl<'a> Parser<'a> {
  pub fn new(sess: &'a ParseSess, mut r: Box<Reader>) -> Parser<'a> {
    let tok0 = r.real_token();
    let span = tok0.sp;
    let placeholder = TokenAndSpan {
      tok: Token::Underscore,
      sp: span
    };

    Parser {
      sess: sess,
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

  pub fn span_fatal(&self, sp: Span, m: &str) -> DiagnosticBuilder {
    unimplemented!()
  }

  pub fn unexpected_last<T>(&self, t: &token::Token) -> PResult<T> {
    unimplemented!()
  }

  pub fn expect_no_suffix(&self, sp: Span, kind: &str, suffix: Option<ast::Name>) {
    unimplemented!()
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

  /// Matches lit = true | false | token_lit
  pub fn parse_lit(&mut self) -> PResult<Lit> {
    let lo = self.span.lo;
    let lit = if self.eat_keyword(keywords::True) {
      LitKind::Bool(true)
    } else if self.eat_keyword(keywords::False) {
      LitKind::Bool(false)
    } else {
      let lit = self.parse_lit_token()?;
      lit
    };
    Ok(codemap::Spanned { node: lit, span: mk_span(lo, self.last_span.hi) })
  }

  /// Matches token_lit = LIT_INTEGER | ...
  pub fn parse_lit_token(&mut self) -> PResult<LitKind> {
    let out = match self.token {
      token::Literal(lit, suf) => {
        let (suffix_illegal, out) = match lit {
          token::Byte(i) => (true, LitKind::Byte(byte_lit(&i.as_str()).0)),
          token::Char(i) => (true, LitKind::Char(char_lit(&i.as_str()).0)),

          // there are some valid suffixes for integer and
          // float literals, so all the handling is done
          // internally.
          token::Integer(s) => {
            (false, integer_lit(&s.as_str(),
                                       suf.as_ref().map(|s| s.as_str()),
                                       &self.sess.span_diagnostic,
                                       self.span))
          }
          token::Float(s) => {
            (false, float_lit(&s.as_str(),
                                     suf.as_ref().map(|s| s.as_str()),
                                     &self.sess.span_diagnostic,
                                     self.span))
          }

          token::Str_(s) => {
            (true,
              LitKind::Str(token::intern_and_get_ident(&str_lit(&s.as_str())),
                           ast::StrStyle::Cooked))
          }
          token::StrRaw(s, n) => {
            (true,
              LitKind::Str(
                token::intern_and_get_ident(&raw_str_lit(&s.as_str())),
                ast::StrStyle::Raw(n)))
          }
          token::ByteStr(i) =>
            (true, LitKind::ByteStr(byte_str_lit(&i.as_str()))),
          token::ByteStrRaw(i, _) =>
            (true,
              LitKind::ByteStr(Rc::new(i.to_string().into_bytes()))),
        };

        if suffix_illegal {
          let sp = self.span;
          self.expect_no_suffix(sp, &format!("{} literal", lit.short_name()), suf)
        }

        out
      }
      _ => { return self.unexpected_last(&self.token); }
    };

    self.bump();
    Ok(out)
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

/// Parse a string representing a character literal into its final form.
/// Rather than just accepting/rejecting a given literal, unescapes it as
/// well. Can take any slice prefixed by a character escape. Returns the
/// character and the number of characters consumed.
pub fn char_lit(lit: &str) -> (char, isize) {
  use std::char;

  let mut chars = lit.chars();
  let c = match (chars.next(), chars.next()) {
    (Some(c), None) if c != '\\' => return (c, 1),
    (Some('\\'), Some(c)) => match c {
      '"' => Some('"'),
      'n' => Some('\n'),
      'r' => Some('\r'),
      't' => Some('\t'),
      '\\' => Some('\\'),
      '\'' => Some('\''),
      '0' => Some('\0'),
      _ => { None }
    },
    _ => panic!("lexer accepted invalid char escape `{}`", lit)
  };

  match c {
    Some(x) => return (x, 2),
    None => { }
  }

  let msg = format!("lexer should have rejected a bad character escape {}", lit);
  let msg2 = &msg[..];

  fn esc(len: usize, lit: &str) -> Option<(char, isize)> {
    u32::from_str_radix(&lit[2..len], 16).ok()
    .and_then(char::from_u32)
    .map(|x| (x, len as isize))
  }

  let unicode_escape = || -> Option<(char, isize)> {
    if lit.as_bytes()[2] == b'{' {
      let idx = lit.find('}').expect(msg2);
      let subslice = &lit[3..idx];
      u32::from_str_radix(subslice, 16).ok()
      .and_then(char::from_u32)
      .map(|x| (x, subslice.chars().count() as isize + 4))
    } else {
      esc(6, lit)
    }
  };

  // Unicode escapes
  return match lit.as_bytes()[1] as char {
    'x' | 'X' => esc(4, lit),
    'u' => unicode_escape(),
    'U' => esc(10, lit),
    _ => None,
  }.expect(msg2);
}

/// Parse a string representing a string literal into its final form. Does
/// unescaping.
pub fn str_lit(lit: &str) -> String {
  debug!("parse_str_lit: given {}", lit.escape_default());
  let mut res = String::with_capacity(lit.len());

  // FIXME #8372: This could be a for-loop if it didn't borrow the iterator
  let error = |i| format!("lexer should have rejected {} at {}", lit, i);

  /// Eat everything up to a non-whitespace
  fn eat<'a>(it: &mut iter::Peekable<str::CharIndices<'a>>) {
    loop {
      match it.peek().map(|x| x.1) {
        Some(' ') | Some('\n') | Some('\r') | Some('\t') => {
          it.next();
        },
        _ => { break; }
      }
    }
  }

  let mut chars = lit.char_indices().peekable();
  loop {
    match chars.next() {
      Some((i, c)) => {
        match c {
          '\\' => {
            let ch = chars.peek().unwrap_or_else(|| {
              panic!("{}", error(i))
            }).1;

            if ch == '\n' {
              eat(&mut chars);
            } else if ch == '\r' {
              chars.next();
              let ch = chars.peek().unwrap_or_else(|| {
                panic!("{}", error(i))
              }).1;

              if ch != '\n' {
                panic!("lexer accepted bare CR");
              }
              eat(&mut chars);
            } else {
              // otherwise, a normal escape
              let (c, n) = char_lit(&lit[i..]);
              for _ in 0..n - 1 { // we don't need to move past the first \
                chars.next();
              }
              res.push(c);
            }
          },
          '\r' => {
            let ch = chars.peek().unwrap_or_else(|| {
              panic!("{}", error(i))
            }).1;

            if ch != '\n' {
              panic!("lexer accepted bare CR");
            }
            chars.next();
            res.push('\n');
          }
          c => res.push(c),
        }
      },
      None => break
    }
  }

  res.shrink_to_fit(); // probably not going to do anything, unless there was an escape.
  debug!("parse_str_lit: returning {}", res);
  res
}

/// Parse a string representing a raw string literal into its final form. The
/// only operation this does is convert embedded CRLF into a single LF.
pub fn raw_str_lit(lit: &str) -> String {
  debug!("raw_str_lit: given {}", lit.escape_default());
  let mut res = String::with_capacity(lit.len());

  // FIXME #8372: This could be a for-loop if it didn't borrow the iterator
  let mut chars = lit.chars().peekable();
  loop {
    match chars.next() {
      Some(c) => {
        if c == '\r' {
          if *chars.peek().unwrap() != '\n' {
            panic!("lexer accepted bare CR");
          }
          chars.next();
          res.push('\n');
        } else {
          res.push(c);
        }
      },
      None => break
    }
  }

  res.shrink_to_fit();
  res
}

// check if `s` looks like i32 or u1234 etc.
fn looks_like_width_suffix(first_chars: &[char], s: &str) -> bool {
  s.len() > 1 &&
  first_chars.contains(&char_at(s, 0)) &&
  s[1..].chars().all(|c| '0' <= c && c <= '9')
}

fn filtered_float_lit(data: token::InternedString, suffix: Option<&str>,
  sd: &Handler, sp: Span) -> ast::LitKind {
  debug!("filtered_float_lit: {}, {:?}", data, suffix);
  match suffix.as_ref().map(|s| &**s) {
    Some("f32") => ast::LitKind::Float(data, ast::FloatTy::F32),
    Some("f64") => ast::LitKind::Float(data, ast::FloatTy::F64),
    Some(suf) => {
      if suf.len() >= 2 && looks_like_width_suffix(&['f'], suf) {
        // if it looks like a width, lets try to be helpful.
        sd.struct_span_err(sp, &format!("invalid width `{}` for float literal", &suf[1..]))
        .help("valid widths are 32 and 64")
        .emit();
      } else {
        sd.struct_span_err(sp, &format!("invalid suffix `{}` for float literal", suf))
        .help("valid suffixes are `f32` and `f64`")
        .emit();
      }

      ast::LitKind::FloatUnsuffixed(data)
    }
    None => ast::LitKind::FloatUnsuffixed(data)
  }
}

pub fn float_lit(s: &str, suffix: Option<InternedString>,
  sd: &Handler, sp: Span) -> ast::LitKind {
  debug!("float_lit: {:?}, {:?}", s, suffix);
  // FIXME #2252: bounds checking float literals is deferred until trans
  let s = s.chars().filter(|&c| c != '_').collect::<String>();
  let data = token::intern_and_get_ident(&s);
  filtered_float_lit(data, suffix.as_ref().map(|s| &**s), sd, sp)
}

/// Parse a string representing a byte literal into its final form. Similar to `char_lit`
pub fn byte_lit(lit: &str) -> (u8, usize) {
  let err = |i| format!("lexer accepted invalid byte literal {} step {}", lit, i);

  if lit.len() == 1 {
    (lit.as_bytes()[0], 1)
  } else {
    assert!(lit.as_bytes()[0] == b'\\', err(0));
    let b = match lit.as_bytes()[1] {
      b'"' => b'"',
      b'n' => b'\n',
      b'r' => b'\r',
      b't' => b'\t',
      b'\\' => b'\\',
      b'\'' => b'\'',
      b'0' => b'\0',
      _ => {
        match u64::from_str_radix(&lit[2..4], 16).ok() {
          Some(c) =>
            if c > 0xFF {
              panic!(err(2))
            } else {
              return (c as u8, 4)
            },
          None => panic!(err(3))
        }
      }
    };
    return (b, 2);
  }
}

pub fn byte_str_lit(lit: &str) -> Rc<Vec<u8>> {
  let mut res = Vec::with_capacity(lit.len());

  // FIXME #8372: This could be a for-loop if it didn't borrow the iterator
  let error = |i| format!("lexer should have rejected {} at {}", lit, i);

  /// Eat everything up to a non-whitespace
  fn eat<'a, I: Iterator<Item=(usize, u8)>>(it: &mut iter::Peekable<I>) {
    loop {
      match it.peek().map(|x| x.1) {
        Some(b' ') | Some(b'\n') | Some(b'\r') | Some(b'\t') => {
          it.next();
        },
        _ => { break; }
      }
    }
  }

  // byte string literals *must* be ASCII, but the escapes don't have to be
  let mut chars = lit.bytes().enumerate().peekable();
  loop {
    match chars.next() {
      Some((i, b'\\')) => {
        let em = error(i);
        match chars.peek().expect(&em).1 {
          b'\n' => eat(&mut chars),
          b'\r' => {
            chars.next();
            if chars.peek().expect(&em).1 != b'\n' {
              panic!("lexer accepted bare CR");
            }
            eat(&mut chars);
          }
          _ => {
            // otherwise, a normal escape
            let (c, n) = byte_lit(&lit[i..]);
            // we don't need to move past the first \
            for _ in 0..n - 1 {
              chars.next();
            }
            res.push(c);
          }
        }
      },
      Some((i, b'\r')) => {
        let em = error(i);
        if chars.peek().expect(&em).1 != b'\n' {
          panic!("lexer accepted bare CR");
        }
        chars.next();
        res.push(b'\n');
      }
      Some((_, c)) => res.push(c),
      None => break,
    }
  }

  Rc::new(res)
}

pub fn integer_lit(s: &str,
                   suffix: Option<InternedString>,
                   sd: &Handler,
                   sp: Span)
                   -> ast::LitKind {
  // s can only be ascii, byte indexing is fine

  let s2 = s.chars().filter(|&c| c != '_').collect::<String>();
  let mut s = &s2[..];

  debug!("integer_lit: {}, {:?}", s, suffix);

  let mut base = 10;
  let orig = s;
  let mut ty = ast::LitIntType::Unsuffixed;

  if char_at(s, 0) == '0' && s.len() > 1 {
    match char_at(s, 1) {
      'x' => base = 16,
      'o' => base = 8,
      'b' => base = 2,
      _ => { }
    }
  }

  // 1f64 and 2f32 etc. are valid float literals.
  if let Some(ref suf) = suffix {
    if looks_like_width_suffix(&['f'], suf) {
      match base {
        16 => sd.span_err(sp, "hexadecimal float literal is not supported"),
        8 => sd.span_err(sp, "octal float literal is not supported"),
        2 => sd.span_err(sp, "binary float literal is not supported"),
        _ => ()
      }
      let ident = token::intern_and_get_ident(&s);
      return filtered_float_lit(ident, Some(&suf), sd, sp)
    }
  }

  if base != 10 {
    s = &s[2..];
  }

  if let Some(ref suf) = suffix {
    if suf.is_empty() { sd.span_bug(sp, "found empty literal suffix in Some")}
    ty = match &**suf {
      "isize" => ast::LitIntType::Signed(ast::IntTy::Is),
      "i8"  => ast::LitIntType::Signed(ast::IntTy::I8),
      "i16" => ast::LitIntType::Signed(ast::IntTy::I16),
      "i32" => ast::LitIntType::Signed(ast::IntTy::I32),
      "i64" => ast::LitIntType::Signed(ast::IntTy::I64),
      "usize" => ast::LitIntType::Unsigned(ast::UintTy::Us),
      "u8"  => ast::LitIntType::Unsigned(ast::UintTy::U8),
      "u16" => ast::LitIntType::Unsigned(ast::UintTy::U16),
      "u32" => ast::LitIntType::Unsigned(ast::UintTy::U32),
      "u64" => ast::LitIntType::Unsigned(ast::UintTy::U64),
      _ => {
        // i<digits> and u<digits> look like widths, so lets
        // give an error message along those lines
        if looks_like_width_suffix(&['i', 'u'], suf) {
          sd.struct_span_err(sp, &format!("invalid width `{}` for integer literal",
          &suf[1..]))
          .help("valid widths are 8, 16, 32 and 64")
          .emit();
        } else {
          sd.struct_span_err(sp, &format!("invalid suffix `{}` for numeric literal", suf))
          .help("the suffix must be one of the integral types \
                             (`u32`, `isize`, etc)")
          .emit();
        }

        ty
      }
    }
  }

  debug!("integer_lit: the type is {:?}, base {:?}, the new string is {:?}, the original \
           string was {:?}, the original suffix was {:?}", ty, base, s, orig, suffix);

  match u64::from_str_radix(s, base) {
    Ok(r) => ast::LitKind::Int(r, ty),
    Err(_) => {
      // small bases are lexed as if they were base 10, e.g, the string
      // might be `0b10201`. This will cause the conversion above to fail,
      // but these cases have errors in the lexer: we don't want to emit
      // two errors, and we especially don't want to emit this error since
      // it isn't necessarily true.
      let already_errored = base < 10 &&
      s.chars().any(|c| c.to_digit(10).map_or(false, |d| d >= base));

      if !already_errored {
        sd.span_err(sp, "int literal is too large");
      }
      ast::LitKind::Int(0, ty)
    }
  }
}

#[cfg(test)]
mod tests {
  use token::{self, str_to_ident};
  use std::rc::Rc;
  use lexer::{Reader, StringReader};
  use super::{ParseSess, Parser};

  fn reader(src: &str) -> Box<Reader> {
    Box::new(StringReader::new(Rc::new(src.to_string())))
  }
  #[test]
  fn test_expr() {
    let sess = ParseSess::new();
    let mut r = reader("a + 2");
    let mut p = Parser::new(&sess, r);
    p.parse_expr();
  }
}
