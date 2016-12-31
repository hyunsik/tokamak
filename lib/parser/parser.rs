use std::cell::RefCell;
use std::iter;
use std::mem;
use std::path::{Path, PathBuf};
use std::slice;
use std::str;
use std::rc::Rc;
use std::result::Result;

use abi::{self, Abi};
use ast::{self, Attribute, Module, Item, Package, Constness, Unsafety, Visibility};
use ast::{UnOp, BinOpKind, CaptureBy, Expr, ExprKind, Lit, LitKind, Ident, RangeLimits};
use ast::{ItemKind, ForeignItem, ForeignItemKind};
use ast::{ViewPath, ViewPathList, ViewPathGlob, ViewPathSimple};
use ast::{Ty, TyKind};
use ast::{Block, BlockCheckMode, Stmt, StmtKind, Local, Mutability, UnsafeSource};
use ast::{Arm, BindingMode, Field, Pat, PatKind};
use ast::{Arg, FnDecl, FunctionRetTy};
use ast::Mutability::*;
use codemap::{self, CodeMap, Spanned, spanned};
use common::codespan::{self, BytePos, FileMap, mk_span, Span};
use errors::{DiagnosticBuilder, Handler};
use errors::emitter::ColorConfig;
use lexer::{char_at, Reader, StringReader, TokenAndSpan};
use precedence::{AssocOp, Fixity};
use ptr::P;
use util::ThinVec;
use ttreader;
use token::{self, keywords, InternedString, Token};
use tokenstream::{self, TokenTree, Delimited};

bitflags! {
    pub flags Restrictions: u8 {
        const RESTRICTION_STMT_EXPR         = 1 << 0,
        const RESTRICTION_NO_STRUCT_LITERAL = 1 << 1,
        const NO_NONINLINE_MOD  = 1 << 2,
    }
}

type ItemInfo = (Ident, ItemKind, Option<Vec<Attribute>>);

/// Info about a parsing session.
pub struct ParseSess {
  pub span_diagnostic: Handler, // better be the same as the one in the reader!
  /// Used to determine and report recursive mod inclusions
  included_mod_stack: RefCell<Vec<PathBuf>>,
  code_map: Rc<CodeMap>,
}

impl ParseSess {
  pub fn new() -> ParseSess {
    let cm = Rc::new(CodeMap::new());
    let handler = Handler::with_tty_emitter(ColorConfig::Auto,
                                            true,
                                            false,
                                            Some(cm.clone()));
    ParseSess::with_span_handler(handler, cm)
  }

  pub fn with_span_handler(handler: Handler, code_map: Rc<CodeMap>) -> ParseSess {
    ParseSess {
      span_diagnostic: handler,
      included_mod_stack: RefCell::new(vec![]),
      code_map: code_map
    }
  }

  pub fn span_diagnostic<'a>(&'a self) -> &'a Handler {
    &self.span_diagnostic
  }

  pub fn codemap(&self) -> &CodeMap {
    &self.code_map
  }
}

#[derive(Clone, Copy, PartialEq)]
pub enum SemiColonMode {
  Break,
  Ignore,
}

pub enum LhsExpr {
  NotYetParsed,
  AttributesParsed(ThinVec<Attribute>),
  AlreadyParsed(P<Expr>),
}

impl From<Option<ThinVec<Attribute>>> for LhsExpr {
  fn from(o: Option<ThinVec<Attribute>>) -> Self {
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

/// SeqSep : a sequence separator (token)
/// and whether a trailing separator is allowed.
pub struct SeqSep {
  pub sep: Option<token::Token>,
  pub trailing_sep_allowed: bool,
}

impl SeqSep {
  pub fn trailing_allowed(t: token::Token) -> SeqSep {
    SeqSep {
      sep: Some(t),
      trailing_sep_allowed: true,
    }
  }

  pub fn none() -> SeqSep {
    SeqSep {
      sep: None,
      trailing_sep_allowed: false,
    }
  }
}

fn maybe_append(mut lhs: Vec<Attribute>, rhs: Option<Vec<Attribute>>) -> Vec<Attribute> {
  if let Some(ref attrs) = rhs {
    lhs.extend(attrs.iter().cloned())
  }
  lhs
}

pub struct Parser<'a> {
  pub sess: &'a ParseSess,
  pub reader: Box<Reader + 'a>,

  /// the current token
  pub token: token::Token,
  /// the span of the current token
  pub span: Span,
  /// the previous token or None (only stashed sometimes).
  pub last_token: Option<Box<token::Token>>,
  last_token_eof: bool,
  /// the span of the prior token
  pub last_span: Span,
  parsing_token_tree: bool,

  pub expected_tokens: Vec<TokenType>,
  pub restrictions: Restrictions,
  /// Stack of open delimiters and their spans. Used for error message.
  pub open_braces: Vec<(token::DelimToken, Span)>,
  pub quote_depth: usize, // not (yet) related to the quasiquoter

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

impl TokenType {
  fn to_string(&self) -> String {
    match *self {
      TokenType::Token(ref t) => format!("`{}`", token::token_to_string(t)),
      TokenType::Operator => "an operator".to_string(),
      TokenType::Keyword(kw) => format!("`{}`", kw.name()),
    }
  }
}

pub type PResult<'a, T> = Result<T, DiagnosticBuilder<'a>>;

impl<'a> Parser<'a> {
  pub fn new(sess: &'a ParseSess, mut r: Box<Reader + 'a>) -> Parser<'a> {
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
      last_token_eof: false,
      last_span: span,
      parsing_token_tree: false,
      buffer: [
        placeholder.clone(),
        placeholder.clone(),
        placeholder.clone(),
        placeholder.clone(),
      ],
      expected_tokens: Vec::new(),
      restrictions: Restrictions::empty(),
      open_braces: Vec::new(),
      quote_depth: 0,

      buffer_start: 0,
      buffer_end: 0,
      tokens_consumed: 0
    }
  }

  //-------------------------------------------------------------------------
  // Error Handler API Section
  //-------------------------------------------------------------------------

  pub fn diagnostic(&self) -> &'a Handler {
    &self.sess.span_diagnostic
  }

  pub fn span_err(&self, sp: Span, m: &str) {
    self.sess.span_diagnostic.span_err(sp, m)
  }

  pub fn fatal(&self, m: &str) -> DiagnosticBuilder<'a> {
    self.sess.span_diagnostic.struct_span_fatal(self.span, m)
  }

  #[allow(unused_variables)]
  pub fn span_fatal(&self, sp: Span, m: &str) -> DiagnosticBuilder<'a> {
    self.sess.span_diagnostic.struct_span_fatal(sp, m)
  }

  pub fn span_fatal_help(&self, sp: Span, m: &str, help: &str) -> DiagnosticBuilder<'a> {
    let mut err = self.sess.span_diagnostic.struct_span_fatal(sp, m);
    err.help(help);
    err
  }

  pub fn bug(&self, m: &str) -> ! {
    self.sess.span_diagnostic.span_bug(self.span, m)
  }

  pub fn span_bug(&self, sp: Span, m: &str) -> ! {
    self.sess.span_diagnostic.span_bug(sp, m)
  }

  pub fn unexpected<T>(&mut self) -> PResult<'a, T> {
    match self.expect_one_of(&[], &[]) {
      Err(e) => Err(e),
      Ok(_) => unreachable!(),
    }
  }

  pub fn unexpected_last<T>(&self, t: &token::Token) -> PResult<'a, T> {
    let token_str = token::token_to_string(t);
    Err(self.span_fatal(self.last_span, &format!("unexpected token: `{}`", token_str)))
  }

  //---------------------------------------------------------------------------
  // Assertion API Section
  //---------------------------------------------------------------------------

  /// Check if the next token is `tok`, and return `true` if so.
  ///
  /// This method is will automatically add `tok` to `expected_tokens` if `tok` is not
  /// encountered.
  pub fn check(&mut self, tok: &token::Token) -> bool {
    let is_present = self.token == *tok;
    if !is_present { self.expected_tokens.push(TokenType::Token(tok.clone())); }
    is_present
  }

  pub fn check_keyword(&mut self, kw: keywords::Keyword) -> bool {
    self.expected_tokens.push(TokenType::Keyword(kw));
    self.token.is_keyword(kw)
  }

  /// Signal an error if the given string is a strict keyword
  pub fn check_strict_keywords(&mut self) {
    if self.token.is_strict_keyword() {
      let token_str = self.this_token_to_string();
      let span = self.span;
      self.span_err(span,
                    &format!("expected identifier, found keyword `{}`",
                    token_str));
    }
  }

  /// Signal an error if the current token is a reserved keyword
  pub fn check_reserved_keywords(&mut self) {
    if self.token.is_reserved_keyword() {
      let token_str = self.this_token_to_string();
      self.fatal(&format!("`{}` is a reserved keyword", token_str)).emit()
    }
  }

  #[allow(unused_variables)]
  pub fn expect_no_suffix(&self, sp: Span, kind: &str, suffix: Option<ast::Name>) {
    match suffix {
      None => {/* everything ok */}
      Some(suf) => {
        let text = suf.as_str();
        if text.is_empty() {
          self.span_bug(sp, "found empty literal suffix in Some")
        }
        self.span_err(sp, &format!("{} with a suffix is invalid", kind));
      }
    }
  }

  /// Expect and consume the token t. Signal an error if
  /// the next token is not t.
  pub fn expect(&mut self, t: &token::Token) -> PResult<'a, ()> {
    if self.expected_tokens.is_empty() {
      if self.token == *t {
        self.bump();
        Ok(())
      } else {
        let token_str = token::token_to_string(t);
        let this_token_str = self.this_token_to_string();
        Err(self.fatal(&format!("expected `{}`, found `{}`", token_str, this_token_str)))
      }
    } else {
      self.expect_one_of(unsafe { slice::from_raw_parts(t, 1) }, &[])
    }
  }

  /// If the given word is not a keyword, signal an error.
  /// If the next token is not the given word, signal an error.
  /// Otherwise, eat it.
  pub fn expect_keyword(&mut self, kw: keywords::Keyword) -> PResult<'a, ()> {
    if !self.eat_keyword(kw) {
      self.unexpected()
    } else {
      Ok(())
    }
  }

  /// Expect next token to be edible or inedible token.  If edible,
  /// then consume it; if inedible, then return without consuming
  /// anything.  Signal a fatal error if next token is unexpected.
  pub fn expect_one_of(&mut self,
                       edible: &[token::Token],
                       inedible: &[token::Token]) -> PResult<'a, ()>{

    fn tokens_to_string(tokens: &[TokenType]) -> String {
      let mut i = tokens.iter();
      // This might be a sign we need a connect method on Iterator.
      let b = i.next()
               .map_or("".to_string(), |t| t.to_string());

      i.enumerate().fold(b, |mut b, (i, ref a)| {
        if tokens.len() > 2 && i == tokens.len() - 2 {
          b.push_str(", or ");
        } else if tokens.len() == 2 && i == tokens.len() - 2 {
          b.push_str(" or ");
        } else {
          b.push_str(", ");
        }
        b.push_str(&a.to_string());
        b
      })
    }

    if edible.contains(&self.token) {
      self.bump();
      Ok(())
    } else if inedible.contains(&self.token) {
      // leave it in the input
      Ok(())
    } else {
      let mut expected = edible.iter()
                               .map(|x| TokenType::Token(x.clone()))
                               .chain(inedible.iter().map(|x| TokenType::Token(x.clone())))
                               .chain(self.expected_tokens.iter().cloned())
                               .collect::<Vec<_>>();
      expected.sort_by(|a, b| a.to_string().cmp(&b.to_string()));
      expected.dedup();
      let expect = tokens_to_string(&expected[..]);
      let actual = self.this_token_to_string();
      Err(self.fatal(
        &(if expected.len() > 1 {
          (format!("expected one of {}, found `{}`",
          expect,
          actual))
        } else if expected.is_empty() {
          (format!("unexpected token: `{}`",
          actual))
        } else {
          (format!("expected {}, found `{}`",
          expect,
          actual))
        })[..]
      ))
    }
  }

  /// Commit to parsing a complete statement `s`, which expects to be
  /// followed by some token from the set edible + inedible.  Check
  /// for recoverable input errors, discarding erroneous characters.
  pub fn commit_stmt(&mut self, edible: &[token::Token],
                     inedible: &[token::Token]) -> PResult<'a, ()> {
    if self.last_token
    .as_ref()
    .map_or(false, |t| t.is_ident() || t.is_path()) {
      let expected = edible.iter()
      .cloned()
      .chain(inedible.iter().cloned())
      .collect::<Vec<_>>();
      self.check_for_erroneous_unit_struct_expecting(&expected);
    }
    self.expect_one_of(edible, inedible)
  }

  pub fn commit_stmt_expecting(&mut self, edible: token::Token) -> PResult<'a, ()> {
    self.commit_stmt(&[edible], &[])
  }

  /// Commit to parsing a complete expression `e` expected to be
  /// followed by some token from the set edible + inedible.  Recover
  /// from anticipated input errors, discarding erroneous characters.
  pub fn commit_expr(&mut self, e: &Expr, edible: &[token::Token],
                     inedible: &[token::Token]) -> PResult<'a, ()> {
    debug!("commit_expr {:?}", e);
    if let ExprKind::Path(..) = e.node {
      // might be unit-struct construction; check for recoverableinput error.
      let expected = edible.iter()
      .cloned()
      .chain(inedible.iter().cloned())
      .collect::<Vec<_>>();
      self.check_for_erroneous_unit_struct_expecting(&expected[..]);
    }
    self.expect_one_of(edible, inedible)
  }

  pub fn commit_expr_expecting(&mut self, e: &Expr, edible: token::Token) -> PResult<'a, ()> {
    self.commit_expr(e, &[edible], &[])
  }

  /// Check for erroneous `ident { }`; if matches, signal error and
  /// recover (without consuming any expected input token).  Returns
  /// true if and only if input was consumed for recovery.
  pub fn check_for_erroneous_unit_struct_expecting(&mut self,
                                                   expected: &[token::Token])
                                                   -> bool {
    if self.token == token::OpenDelim(token::Brace)
        && expected.iter().all(|t| *t != token::OpenDelim(token::Brace))
        && self.look_ahead(1, |t| *t == token::CloseDelim(token::Brace)) {
      // matched; signal non-fatal error and recover.
      let span = self.span;
      self.span_err(span, "unit-like struct construction is written with no trailing `{ }`");
      self.eat(&token::OpenDelim(token::Brace));
      self.eat(&token::CloseDelim(token::Brace));
      true
    } else {
      false
    }
  }

  //-------------------------------------------------------------------------
  // Token Handling API Section
  //-------------------------------------------------------------------------

  /// Advance the parser by one token
  pub fn bump(&mut self) {
    debug!("called bump - token: {}", &token::token_to_string(&self.token));
    if self.last_token_eof {
      // Bumping after EOF is a bad sign, usually an infinite loop.
      self.bug("attempted to bump the parser past EOF (may be stuck in a loop)");
    }

    if self.token == token::Eof {
      self.last_token_eof = true;
    }

    self.last_span = self.span;
    // Stash token for error recovery (sometimes; clone is not necessarily cheap).
    self.last_token = if self.token.is_ident() ||
                         self.token.is_path() ||
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

  pub fn buffer_length(&mut self) -> isize {
    if self.buffer_start <= self.buffer_end {
      return self.buffer_end - self.buffer_start;
    }
    return (4 - self.buffer_start) + self.buffer_end;
  }

  pub fn look_ahead<R, F>(&mut self, distance: usize, f: F) -> R where
  F: FnOnce(&token::Token) -> R,
  {
    let dist = distance as isize;
    while self.buffer_length() < dist {
      self.buffer[self.buffer_end as usize] = self.reader.real_token();
      self.buffer_end = (self.buffer_end + 1) & 3;
    }
    f(&self.buffer[((self.buffer_start + dist - 1) & 3) as usize].tok)
  }

  /// Convert the current token to a string using self's reader
  pub fn this_token_to_string(&self) -> String {
    token::token_to_string(&self.token)
  }

  pub fn this_token_descr(&self) -> String {
    let s = self.this_token_to_string();
    if self.token.is_strict_keyword() {
      format!("keyword `{}`", s)
    } else if self.token.is_reserved_keyword() {
      format!("reserved keyword `{}`", s)
    } else {
      format!("`{}`", s)
    }
  }

  /// Consume token 'tok' if it exists. Returns true if the given
  /// token was present, false otherwise.
  pub fn eat(&mut self, tok: &token::Token) -> bool {
    let is_present = self.check(tok);
    if is_present { self.bump() }
    is_present
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

  /// Eat and discard tokens until one of `kets` is encountered. Respects token trees,
  /// passes through any errors encountered. Used for error recovery.
  pub fn eat_to_tokens(&mut self, kets: &[&token::Token]) {
    self.parse_seq_to_before_tokens(kets,
                                    SeqSep::none(),
                                    |p| p.parse_token_tree(),
                                    |mut e| e.cancel());
  }

  //-------------------------------------------------------------------------
  // Common Parsing API Section
  //-------------------------------------------------------------------------

  // `fe` is an error handler.
  fn parse_seq_to_before_tokens<T, F, Fe>(&mut self,
                                          kets: &[&token::Token],
                                          sep: SeqSep,
                                          mut f: F,
                                          mut fe: Fe)
                                          -> Vec<T>
    where F: FnMut(&mut Parser<'a>) -> PResult<'a, T>,
    Fe: FnMut(DiagnosticBuilder)
  {
    let mut first: bool = true;
    let mut v = vec!();
    while !kets.contains(&&self.token) {
      match sep.sep {
        Some(ref t) => {
          if first {
            first = false;
          } else {
            if let Err(e) = self.expect(t) {
              fe(e);
              break;
            }
          }
        }
        _ => ()
      }
      if sep.trailing_sep_allowed && kets.iter().any(|k| self.check(k)) {
        break;
      }

      match f(self) {
        Ok(t) => v.push(t),
        Err(e) => {
          fe(e);
          break;
        }
      }
    }

    v
  }

  /// Parse a sequence, including the closing delimiter. The function
    /// f must consume tokens until reaching the next separator or
    /// closing bracket.
  pub fn parse_seq_to_end<T, F>(&mut self,
                                ket: &token::Token,
                                sep: SeqSep,
                                f: F)
                                -> PResult<'a, Vec<T>> where
                                  F: FnMut(&mut Parser<'a>) -> PResult<'a,  T>,
  {
    let val = self.parse_seq_to_before_end(ket, sep, f);
    self.bump();
    Ok(val)
  }

  /// Parse a sequence, not including the closing delimiter. The function
    /// f must consume tokens until reaching the next separator or
    /// closing bracket.
  pub fn parse_seq_to_before_end<T, F>(&mut self,
                                       ket: &token::Token,
                                       sep: SeqSep,
                                       f: F)
                                       -> Vec<T>
                                       where F: FnMut(&mut Parser<'a>) -> PResult<'a,  T>
  {
    self.parse_seq_to_before_tokens(&[ket], sep, f, |mut e| e.emit())
  }

  /// Parse a sequence, including the closing delimiter. The function
  /// f must consume tokens until reaching the next separator or
  /// closing bracket.
  pub fn parse_unspanned_seq<T, F>(&mut self,
                                   bra: &token::Token,
                                   ket: &token::Token,
                                   sep: SeqSep,
                                   f: F)
                                   -> PResult<'a, Vec<T>>
    where F: FnMut(&mut Parser<'a>) -> PResult<'a, T>
  {
    self.expect(bra)?;
    let result = self.parse_seq_to_before_end(ket, sep, f);
    if self.token == *ket {
      self.bump();
    }
    Ok(result)
  }

  //-------------------------------------------------------------------------
  // Grammar Section
  //-------------------------------------------------------------------------

  pub fn parse_package(&mut self) -> PResult<'a, Package> {
    let lo = self.span.lo;
    Ok(ast::Package {
      module: self.parse_module(&token::Eof, lo)?,
      span: mk_span(lo, self.span.lo),
      attrs: Vec::new(), // TODO - to be filled
    })
  }

  #[allow(unused_variables)]
  pub fn parse_module(&mut self, terminal: &token::Token, inner_lo: BytePos) -> PResult<'a, Module> {
    let mut items = vec![];
    while let Some(item) = self.parse_item()? {
      items.push(item);
    }

    if !self.eat(terminal) {
      let token_str = self.this_token_to_string();
      return Err(self.fatal(&format!("expected item, found `{}`", token_str)));
    }

    let hi = if self.span == codespan::DUMMY_SPAN {
      inner_lo
    } else {
      self.last_span.hi
    };

    Ok(ast::Module {
      inner: mk_span(inner_lo, hi),
      items: items
    })
  }

  pub fn parse_item(&mut self) -> PResult<'a, Option<P<Item>>> {
    self.parse_item_(Vec::new(), false)
  }

  #[allow(unused_variables)]
  /// Parse one of the items allowed by the flags.
  /// NB: this function no longer parses the items inside an
  /// extern crate.
  fn parse_item_(&mut self, attrs: Vec<Attribute>,
                 attributes_allowed: bool) -> PResult<'a, Option<P<Item>>> {

    let lo = self.span.lo;
    let visibility = self.parse_visibility()?;

    if self.eat_keyword(keywords::Import) {
      let item_ = ItemKind::Import(self.parse_view_path()?);
      self.expect(&token::SemiColon)?;

      let last_span = self.last_span;
      let item = self.mk_item(lo,
                              last_span.hi,
                              keywords::Invalid.ident(),
                              item_,
                              visibility,
                              Vec::new());
      return Ok(Some(item));
    }

    if self.eat_keyword(keywords::Extern) {
      let opt_abi = self.parse_opt_abi()?;

      if self.eat_keyword(keywords::Fn) {
        // EXTERN FUNCTION ITEM
        let abi = opt_abi.unwrap_or(Abi::C);
        let (ident, item_, extra_attrs) =
        self.parse_item_fn(Unsafety::Normal, Constness::NotConst, abi)?;
        let last_span = self.last_span;
        let item = self.mk_item(lo,
                                last_span.hi,
                                ident,
                                item_,
                                visibility,
                                maybe_append(attrs, extra_attrs));
        return Ok(Some(item));
      } else if self.check(&token::OpenDelim(token::Brace)) {
        return Ok(Some(self.parse_item_foreign_mod(lo, opt_abi, visibility, attrs)?));
      }

      self.unexpected()?;
    }

    if self.eat_keyword(keywords::Static) {
      // STATIC ITEM
      let m = if self.eat_keyword(keywords::Var) {
        Mutability::Mutable
      } else {
        Mutability::Immutable
      };
      let (ident, item_, extra_attrs) = self.parse_item_const(Some(m))?;
      let last_span = self.last_span;
      let item = self.mk_item(lo,
                              last_span.hi,
                              ident,
                              item_,
                              visibility,
                              maybe_append(attrs, extra_attrs));
      return Ok(Some(item));
    }

    if self.eat_keyword(keywords::Const) {
      if self.check_keyword(keywords::Fn)
          || (self.check_keyword(keywords::Unsafe) &&
              self.look_ahead(1, |t| t.is_keyword(keywords::Fn))) {

        // CONST FUNCTION ITEM
        let unsafety = if self.eat_keyword(keywords::Unsafe) {
          Unsafety::Unsafe
        } else {
          Unsafety::Normal
        };

        self.bump();
        let (ident, item_, extra_attrs) =
        self.parse_item_fn(unsafety, Constness::Const, Abi::Rust)?;
        let last_span = self.last_span;
        let item = self.mk_item(lo,
                                last_span.hi,
                                ident,
                                item_,
                                visibility,
                                maybe_append(attrs, extra_attrs));
        return Ok(Some(item));
      }

      // CONST ITEM
      if self.eat_keyword(keywords::Var) {
        let last_span = self.last_span;
        self.diagnostic().struct_span_err(last_span, "const globals cannot be mutable")
            .help("did you mean to declare a static?")
            .emit();
      }
      let (ident, item_, extra_attrs) = self.parse_item_const(None)?;
      let last_span = self.last_span;
      let item = self.mk_item(lo,
                              last_span.hi,
                              ident,
                              item_,
                              visibility,
                              maybe_append(attrs, extra_attrs));
      return Ok(Some(item));
    }

    if self.eat_keyword(keywords::Type) {
      // TYPE ITEM
      let (ident, item_, extra_attrs) = self.parse_item_type()?;
      let last_span = self.last_span;
      let item = self.mk_item(lo,
                              last_span.hi,
                              ident,
                              item_,
                              visibility,
                              maybe_append(attrs, extra_attrs));
      return Ok(Some(item));
    }

    if self.eat_keyword(keywords::Enum) {

    }

    if self.eat_keyword(keywords::Struct) {

    }

    if self.check_keyword(keywords::Fn) {
      // FUNCTION ITEM
      self.bump();
      let (ident, item_, extra_attrs) =
        self.parse_item_fn(Unsafety::Normal, Constness::NotConst, Abi::Rust)?;
      let last_span = self.last_span;
      let item = self.mk_item(lo,
                              last_span.hi,
                              ident,
                              item_,
                              visibility,
                              maybe_append(Vec::new(), extra_attrs));
      return Ok(Some(item));
    }

    Ok(None)
  }

  pub fn parse_visibility(&mut self) -> PResult<'a, Visibility> {
    if !self.eat_keyword(keywords::Pub) {
      Ok(Visibility::Inherited)
    } else {
      Ok(Visibility::Public)
    }
  }

  /// Matches ViewPath:
  /// MOD_SEP? non_global_path
  /// MOD_SEP? non_global_path as IDENT
  /// MOD_SEP? non_global_path MOD_SEP STAR
  /// MOD_SEP? non_global_path MOD_SEP LBRACE item_seq RBRACE
  /// MOD_SEP? LBRACE item_seq RBRACE
  fn parse_view_path(&mut self) -> PResult<'a, P<ViewPath>> {
    let lo = self.span.lo;
    if self.check(&token::OpenDelim(token::Brace)) || self.is_import_coupler() {
      // `{foo, bar}` or `::{foo, bar}`
      let prefix = ast::Path {
        global: self.eat(&token::ModSep),
        segments: Vec::new(),
        span: mk_span(lo, self.span.hi),
      };
      let items = self.parse_path_list_items()?;
      Ok(P(spanned(lo, self.span.hi, ViewPathList(prefix, items))))
    } else {
      let prefix = self.parse_path()?;
      if self.is_import_coupler() {
        // `foo::bar::{a, b}` or `foo::bar::*`
        self.bump();
        if self.check(&token::BinOp(token::Star)) {
          self.bump();
          Ok(P(spanned(lo, self.span.hi, ViewPathGlob(prefix))))
        } else {
          let items = self.parse_path_list_items()?;
          Ok(P(spanned(lo, self.span.hi, ViewPathList(prefix, items))))
        }
      } else {
        // `foo::bar` or `foo::bar as baz`
        let rename = self.parse_rename()?.
        unwrap_or(prefix.segments.last().unwrap().identifier);
        Ok(P(spanned(lo, self.last_span.hi, ViewPathSimple(rename, prefix))))
      }
    }
  }

  fn parse_path_list_items(&mut self) -> PResult<'a, Vec<ast::PathListItem>> {
    self.parse_unspanned_seq(&token::OpenDelim(token::Brace),
                             &token::CloseDelim(token::Brace),
                             SeqSep::trailing_allowed(token::Comma), |this| {
        let lo = this.span.lo;
        let node = if this.eat_keyword(keywords::SelfValue) {
          let rename = this.parse_rename()?;
          ast::PathListItemKind::Mod { id: ast::DUMMY_NODE_ID, rename: rename }
        } else {
          let ident = this.parse_ident()?;
          let rename = this.parse_rename()?;
          ast::PathListItemKind::Ident { name: ident, rename: rename, id: ast::DUMMY_NODE_ID }
        };
        let hi = this.last_span.hi;
        Ok(spanned(lo, hi, node))
      })
  }

  fn parse_rename(&mut self) -> PResult<'a, Option<Ident>> {
    if self.eat_keyword(keywords::As) {
      self.parse_ident().map(Some)
    } else {
      Ok(None)
    }
  }

  /// Parse `extern` for foreign ABIs
  /// modules.
  ///
  /// `extern` is expected to have been
  /// consumed before calling this method
  ///
  /// # Examples:
  ///
  /// extern "C" {}
  /// extern {}
  #[allow(unused_mut)]
  fn parse_item_foreign_mod(&mut self,
                            lo: BytePos,
                            opt_abi: Option<abi::Abi>,
                            visibility: Visibility,
                            mut attrs: Vec<Attribute>)
                            -> PResult<'a, P<Item>> {
    self.expect(&token::OpenDelim(token::Brace))?;

    let abi = opt_abi.unwrap_or(Abi::C);

    let mut foreign_items = vec![];
    while let Some(item) = self.parse_foreign_item()? {
      foreign_items.push(item);
    }
    self.expect(&token::CloseDelim(token::Brace))?;

    let last_span = self.last_span;
    let m = ast::ForeignMod {
      abi: abi,
      items: foreign_items
    };
    Ok(self.mk_item(lo,
                    last_span.hi,
                    keywords::Invalid.ident(),
                    ItemKind::ForeignMod(m),
                    visibility,
                    attrs))
  }

  /// Parse a foreign item.
  fn parse_foreign_item(&mut self) -> PResult<'a, Option<ForeignItem>> {
    let lo = self.span.lo;
    let visibility = self.parse_visibility()?;

    if self.check_keyword(keywords::Static) {
      // FOREIGN STATIC ITEM
      return Ok(Some(self.parse_item_foreign_static(visibility, lo, Vec::new())?));
    }
    if self.check_keyword(keywords::Fn) {
      // FOREIGN FUNCTION ITEM
      return Ok(Some(self.parse_item_foreign_fn(visibility, lo, Vec::new())?));
    }

    Ok(None)
  }

  /// Parse a function declaration from a foreign module
  fn parse_item_foreign_fn(&mut self, vis: ast::Visibility, lo: BytePos,
                           attrs: Vec<Attribute>) -> PResult<'a, ForeignItem> {
    self.expect_keyword(keywords::Fn)?;

    let ident = self.parse_fn_header()?;
    let decl = self.parse_fn_decl(true)?;
    let hi = self.span.hi;
//    self.expect(&token::SemiColon)?;

    Ok(ast::ForeignItem {
      ident: ident,
      attrs: attrs,
      node: ForeignItemKind::Fn(decl),
      id: ast::DUMMY_NODE_ID,
      span: mk_span(lo, hi),
      vis: vis
    })
  }

  /// Parse a static item from a foreign module
  fn parse_item_foreign_static(&mut self, vis: ast::Visibility, lo: BytePos,
                               attrs: Vec<Attribute>) -> PResult<'a, ForeignItem> {
    self.expect_keyword(keywords::Static)?;
    let mutbl = self.eat_keyword(keywords::Var);

    let ident = self.parse_ident()?;
    self.expect(&token::Colon)?;
    let ty = self.parse_ty()?;
    let hi = self.span.hi;
//    self.expect(&token::SemiColon)?;
    Ok(ForeignItem {
      ident: ident,
      attrs: attrs,
      node: ForeignItemKind::Static(ty, mutbl),
      id: ast::DUMMY_NODE_ID,
      span: mk_span(lo, hi),
      vis: vis
    })
  }

  fn parse_item_const(&mut self, m: Option<Mutability>) -> PResult<'a, ItemInfo> {
    let id = self.parse_ident()?;
    self.expect(&token::Colon)?;
    let ty = self.parse_ty()?;
    self.expect(&token::Eq)?;
    let e = self.parse_expr()?;
    //self.commit_expr_expecting(&e, token::SemiColon)?;
    let item = match m {
      Some(m) => ItemKind::Static(ty, m, e),
      None => ItemKind::Const(ty, e),
    };
    Ok((id, item, None))
  }

  /// Parse type Foo = Bar;
  fn parse_item_type(&mut self) -> PResult<'a, ItemInfo> {
    let ident = self.parse_ident()?;
    self.expect(&token::Eq)?;
    let ty = self.parse_ty()?;
//    self.expect(&token::SemiColon)?;
    Ok((ident, ItemKind::Ty(ty), None))
  }

  /// Parses a string as an ABI spec on an extern type or module. Consumes
  /// the `extern` keyword, if one is found.
  fn parse_opt_abi(&mut self) -> PResult<'a, Option<abi::Abi>> {
    match self.token {
      token::Literal(token::Str_(s), suf) | token::Literal(token::StrRaw(s, _), suf) => {
        let sp = self.span;
        self.expect_no_suffix(sp, "ABI spec", suf);
        self.bump();

        match abi::lookup(&s.as_str()) {
          Some(abi) => Ok(Some(abi)),
          None => {
            let last_span = self.last_span;
            self.span_err(
              last_span,
              &format!("invalid ABI: expected one of [{}], found `{}`",
                abi::all_names().join(", "), s)
            );

            Ok(None)
          }
        }
      }

      _ => Ok(None),
    }
  }

  /// Parse an item-position function declaration.
  fn parse_item_fn(&mut self,
                   unsafety: Unsafety,
                   constness: Constness,
                   abi: abi::Abi)
                   -> PResult<'a, ItemInfo> {
    let ident = self.parse_fn_header()?;
    let decl = self.parse_fn_decl(false)?;
    let (inner_attrs, body) = self.parse_inner_attrs_and_block()?;
    Ok((ident, ItemKind::Fn(decl, unsafety, constness, abi, body), Some(inner_attrs)))
  }

  /// Parse the name and optional generic types of a function header.
  fn parse_fn_header(&mut self) -> PResult<'a, Ident> {
    let id = self.parse_ident()?;
    Ok(id)
  }

  /// Parse the argument list and result type of a function declaration
  pub fn parse_fn_decl(&mut self, allow_variadic: bool) -> PResult<'a, P<FnDecl>> {

    let (args, variadic) = self.parse_fn_args(true, allow_variadic)?;
    let ret_ty = self.parse_ret_ty()?;

    Ok(P(FnDecl {
      inputs: args,
      output: ret_ty,
      variadic: variadic
    }))
  }

  fn parse_fn_args(&mut self, named_args: bool, allow_variadic: bool)
                   -> PResult<'a, (Vec<Arg> , bool)> {

    let sp = self.span;
    let mut variadic = false;
    let args: Vec<Option<Arg>> =
      self.parse_unspanned_seq(
        &token::OpenDelim(token::Paren),
        &token::CloseDelim(token::Paren),
        SeqSep::trailing_allowed(token::Comma),
        |p| {
          if p.token == token::DotDotDot {
            p.bump();
            if allow_variadic {
              if p.token != token::CloseDelim(token::Paren) {
                let span = p.span;
                p.span_err(span,
                           "`...` must be last in argument list for variadic function");
              }
            } else {
              let span = p.span;
              p.span_err(span,
                         "only foreign functions are allowed to be variadic");
            }
            variadic = true;
            Ok(None)
          } else {
            match p.parse_arg_general(named_args) {
              Ok(arg) => Ok(Some(arg)),
              Err(mut e) => {
                e.emit();
                p.eat_to_tokens(&[&token::Comma, &token::CloseDelim(token::Paren)]);
                Ok(None)
              }
            }
          }
        }
      )?;

    let args: Vec<_> = args.into_iter().filter_map(|x| x).collect();

    if variadic && args.is_empty() {
      self.span_err(sp,
                    "variadic function must be declared with at least one named argument");
    }

    Ok((args, variadic))
  }

  /// This version of parse arg doesn't necessarily require
  /// identifier names.
  pub fn parse_arg_general(&mut self, require_name: bool) -> PResult<'a, Arg> {

    let pat = if require_name || self.is_named_argument() {
      debug!("parse_arg_general parse_pat (require_name:{})",
      require_name);
      let pat = self.parse_pat()?;

      self.expect(&token::Colon)?;
      pat
    } else {
      debug!("parse_arg_general ident_to_pat");
      let sp = self.last_span;
      let spanned = Spanned { span: sp, node: keywords::Invalid.ident() };
      P(Pat {
        id: ast::DUMMY_NODE_ID,
        node: PatKind::Ident(BindingMode::ByValue, spanned, None),
        span: sp
      })
    };

    let t = self.parse_ty()?;

    Ok(Arg {
      ty: t,
      pat: pat,
      id: ast::DUMMY_NODE_ID,
    })
  }

  pub fn is_named_argument(&mut self) -> bool {
    let offset = match self.token {
      token::BinOp(token::And) => 1,
      token::AndAnd => 1,
      _ if self.token.is_keyword(keywords::Mut) => 1,
      _ => 0
    };

    debug!("parser is_named_argument offset:{}", offset);

    if offset == 0 {
      is_ident_or_underscore(&self.token)
      && self.look_ahead(1, |t| *t == token::Colon)
    } else {
      self.look_ahead(offset, |t| is_ident_or_underscore(t))
      && self.look_ahead(offset + 1, |t| *t == token::Colon)
    }
  }

  /// Parse optional return type [ -> TY ] in function decl
  pub fn parse_ret_ty(&mut self) -> PResult<'a, FunctionRetTy> {
    if self.eat(&token::RArrow) {
      if self.eat(&token::Not) {
        Ok(FunctionRetTy::None(self.last_span))
      } else {
        Ok(FunctionRetTy::Ty(self.parse_ty()?))
      }
    } else {
      let pos = self.span.lo;
      Ok(FunctionRetTy::Default(mk_span(pos, pos)))
    }
  }

  /// Parse a block. No inner attrs are allowed.
  pub fn parse_block(&mut self) -> PResult<'a, P<Block>> {
    let lo = self.span.lo;

    if !self.eat(&token::OpenDelim(token::Brace)) {
      let sp = self.span;
      let tok = self.this_token_to_string();
      return Err(self.span_fatal_help(sp,
                                      &format!("expected `{{`, found `{}`", tok),
                                      "place this code inside a block"));
    }

    self.parse_block_tail(lo, BlockCheckMode::Default)
  }

  /// Parse a block. Inner attrs are allowed.
  fn parse_inner_attrs_and_block(&mut self) -> PResult<'a, (Vec<Attribute>, P<Block>)> {
    let lo = self.span.lo;
    self.expect(&token::OpenDelim(token::Brace))?;
    Ok((Vec::new(), self.parse_block_tail(lo, BlockCheckMode::Default)?))
  }

  /// Parse the rest of a block expression or function body
  /// Precondition: already parsed the '{'.
  fn parse_block_tail(&mut self, lo: BytePos, s: BlockCheckMode) -> PResult<'a, P<Block>> {
    let mut stmts = vec![];

    while !self.eat(&token::CloseDelim(token::Brace)) {
      if let Some(stmt) = self.parse_full_stmt()? {
        stmts.push(stmt);
      } else if self.token == token::Eof {
        break;
      } else {
        // Found only `;` or `}`.
        continue;
      };
    }

    Ok(P(ast::Block {
      stmts: stmts,
      id: ast::DUMMY_NODE_ID,
      rules: s,
      span: codespan::mk_span(lo, self.last_span.hi),
    }))
  }

  /// Parse a statement, including the trailing semicolon.
  pub fn parse_full_stmt(&mut self) -> PResult<'a, Option<Stmt>> {
    let mut stmt = match self.parse_stmt_() {
      Some(stmt) => stmt,
      None => return Ok(None),
    };

    match stmt.node {
      StmtKind::Expr(ref expr) if self.token != token::Eof => {
        // expression without semicolon
        if expr_requires_semi_to_be_stmt(expr) {
          // Just check for errors and recover; do not eat semicolon yet.
          if let Err(mut e) =
          self.expect_one_of(&[], &[token::SemiColon, token::CloseDelim(token::Brace)])
            {
              e.emit();
              self.recover_stmt();
            }
        }
      }
//      StmtKind::Local(..) => {
//        // We used to incorrectly allow a macro-expanded let statement to lack a semicolon.
//        if self.token != token::SemiColon {
//          self.warn_missing_semicolon();
//        } else {
//          self.expect_one_of(&[token::SemiColon], &[])?;
//        }
//      }
      _ => {}
    }

    if self.eat(&token::SemiColon) {
      stmt = stmt.add_trailing_semicolon();
    }

    stmt.span.hi = self.last_span.hi;
    Ok(Some(stmt))
  }

  fn parse_stmt_(&mut self) -> Option<Stmt> {
    self.parse_stmt_without_recovery().unwrap_or_else(|mut e| {
      e.emit();
      self.recover_stmt_(SemiColonMode::Break);
      None
    })
  }

  fn warn_missing_semicolon(&self) {
    self.diagnostic().struct_span_warn(self.span, {
      &format!("expected `;`, found `{}`", self.this_token_to_string())
    }).note({
      "This was erroneously allowed and will become a hard error in a future release"
    }).emit();
  }

  fn parse_stmt_without_recovery(&mut self) -> PResult<'a, Option<Stmt>> {

    let attrs = Vec::new();
    let lo = self.span.lo;

    Ok(Some(if self.eat_keyword(keywords::Let) {
      Stmt {
        id: ast::DUMMY_NODE_ID,
        node: StmtKind::Local(self.parse_let(attrs.into())?),
        span: codespan::mk_span(lo, self.last_span.hi),
      }
    } else if self.eat_keyword(keywords::Var) {
      Stmt {
        id: ast::DUMMY_NODE_ID,
        node: StmtKind::Local(self.parse_var(attrs.into())?),
        span: codespan::mk_span(lo, self.last_span.hi),
      }
    } else {
      // FIXME: Bad copy of attrs
      let restrictions = self.restrictions | NO_NONINLINE_MOD;
      match self.with_res(restrictions,
                          |this| this.parse_item_(attrs.clone(), true))? {
        Some(i) => Stmt {
          id: ast::DUMMY_NODE_ID,
          span: codespan::mk_span(lo, i.span.hi),
          node: StmtKind::Item(i),
        },
        None => {
          let unused_attrs = |attrs: &[_], s: &mut Self| {
            if attrs.len() > 0 {
              s.span_err(s.span,
                         "expected statement after outer attribute");
            }
          };

          // Do not attempt to parse an expression if we're done here.
          if self.token == token::SemiColon {
            unused_attrs(&attrs, self);
            self.bump();
            return Ok(None);
          }

          if self.token == token::CloseDelim(token::Brace) {
            unused_attrs(&attrs, self);
            return Ok(None);
          }

          // Remainder are line-expr stmts.
          let e = self.parse_expr_res(RESTRICTION_STMT_EXPR, Some(attrs.into()))?;
          Stmt {
            id: ast::DUMMY_NODE_ID,
            span: codespan::mk_span(lo, e.span.hi),
            node: StmtKind::Expr(e),
          }
        }
      }
    }))
  }

  // Eat tokens until we can be relatively sure we reached the end of the
  // statement. This is something of a best-effort heuristic.
  //
  // We terminate when we find an unmatched `}` (without consuming it).
  fn recover_stmt(&mut self) {
    self.recover_stmt_(SemiColonMode::Ignore)
  }

  // If `break_on_semi` is `Break`, then we will stop consuming tokens after
  // finding (and consuming) a `;` outside of `{}` or `[]` (note that this is
  // approximate - it can mean we break too early due to macros, but that
  // shoud only lead to sub-optimal recovery, not inaccurate parsing).
  fn recover_stmt_(&mut self, break_on_semi: SemiColonMode) {
    let mut brace_depth = 0;
    let mut bracket_depth = 0;

    loop {
      debug!("recover_stmt_ loop {:?}", self.token);
      match self.token {
        token::OpenDelim(token::DelimToken::Brace) => {
          brace_depth += 1;
          self.bump();
        }
        token::OpenDelim(token::DelimToken::Bracket) => {
          bracket_depth += 1;
          self.bump();
        }
        token::CloseDelim(token::DelimToken::Brace) => {
          if brace_depth == 0 {
            debug!("recover_stmt_ return - close delim {:?}", self.token);
            return;
          }
          brace_depth -= 1;
          self.bump();
        }
        token::CloseDelim(token::DelimToken::Bracket) => {
          bracket_depth -= 1;
          if bracket_depth < 0 {
            bracket_depth = 0;
          }
          self.bump();
        }
        token::Eof => {
          debug!("recover_stmt_ return - Eof");
          return;
        }
        token::SemiColon => {
          self.bump();
          if break_on_semi == SemiColonMode::Break &&
              brace_depth == 0 &&
              bracket_depth == 0 {
            debug!("recover_stmt_ return - Semi");
            return;
          }
        }
        _ => {
          self.bump()
        }
      }
    }
  }

  /// Parse a "let" stmt
  fn parse_let(&mut self, attrs: ThinVec<Attribute>) -> PResult<'a, P<Local>> {
    self.parse_local(Immutable, attrs)
  }

  /// Parse a "var" stmt
  fn parse_var(&mut self, attrs: ThinVec<Attribute>) -> PResult<'a, P<Local>> {
    self.parse_local(Mutable, attrs)
  }

  /// Parse a local variable declaration
  fn parse_local(&mut self, mutbl: Mutability, attrs: ThinVec<Attribute>) -> PResult<'a, P<Local>> {
    let lo = self.span.lo;
    let pat = self.parse_pat()?;

    let mut ty = None;
    if self.eat(&token::Colon) {
      ty = Some(self.parse_ty()?);
    }
    let init = self.parse_initializer()?;
    Ok(P(ast::Local {
      ty: ty,
      pat: pat,
      mutbl: mutbl,
      init: init,
      id: ast::DUMMY_NODE_ID,
      span: mk_span(lo, self.last_span.hi),
      attrs: attrs,
    }))
  }

  /// Parse the RHS of a local variable declaration (e.g. '= 14;')
  fn parse_initializer(&mut self) -> PResult<'a, Option<P<Expr>>> {
    if self.check(&token::Eq) {
      self.bump();
      Ok(Some(self.parse_expr()?))
    } else {
      Ok(None)
    }
  }

  /// Is this expression a successfully-parsed statement?
  fn expr_is_complete(&mut self, e: &Expr) -> bool {
    self.restrictions.contains(RESTRICTION_STMT_EXPR) &&
      !expr_requires_semi_to_be_stmt(e)
  }

  /// Parse an expression
  pub fn parse_expr(&mut self) -> PResult<'a, P<Expr>> {
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
                        already_parsed_attrs: Option<ThinVec<Attribute>>)
                        -> PResult<'a, P<Expr>> {
    self.with_res(r, |this| this.parse_assoc_expr(already_parsed_attrs))
  }

  /// Parse an associative expression
  ///
  /// This parses an expression accounting for associativity and precedence of the operators in
  /// the expression.
  pub fn parse_assoc_expr(&mut self,
                          already_parsed_attrs: Option<ThinVec<Attribute>>)
                          -> PResult<'a, P<Expr>> {
    self.parse_assoc_expr_with(0, already_parsed_attrs.into())
  }

  /// Parse an associative expression with operators of at least `min_prec` precedence
  pub fn parse_assoc_expr_with(&mut self, min_prec: usize, lhs: LhsExpr)
      -> PResult<'a, P<Expr>> {
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

    if self.expr_is_complete(&lhs) {
      // Semi-statement forms are odd. See https://github.com/rust-lang/rust/issues/29071
      return Ok(lhs);
    }

    self.expected_tokens.push(TokenType::Operator);

    while let Some(op) = AssocOp::from_token(&self.token) {

      let cur_op_span = self.span;
      let restrictions = if op.is_assign_like() {
        self.restrictions & RESTRICTION_NO_STRUCT_LITERAL
      } else {
        self.restrictions
      };
      if op.precedence() < min_prec {
        break;
      }
      self.bump();
      if op.is_comparison() {
        self.check_no_chained_comparison(&lhs, &op);
      }

      // Special cases:
      if op == AssocOp::As {
        let rhs = self.parse_ty()?;
        let (lo, hi) = (lhs.span.lo, rhs.span.hi);
        lhs = self.mk_expr(lo, hi, ExprKind::Cast(lhs, rhs), ThinVec::new());
        continue
      } else if op == AssocOp::Colon {
        let rhs = self.parse_ty()?;
        let (lo, hi) = (lhs.span.lo, rhs.span.hi);
        lhs = self.mk_expr(lo, hi, ExprKind::Type(lhs, rhs), ThinVec::new());
        continue
      } else if op == AssocOp::DotDot || op == AssocOp::DotDotDot {
        // If we didn’t have to handle `x..`/`x...`, it would be pretty easy to
        // generalise it to the Fixity::None code.
        //
        // We have 2 alternatives here: `x..y`/`x...y` and `x..`/`x...` The other
        // two variants are handled with `parse_prefix_range_expr` call above.
        let rhs = if self.is_at_start_of_range_notation_rhs() {
          Some(self.parse_assoc_expr_with(op.precedence() + 1,
                                          LhsExpr::NotYetParsed)?)
        } else {
          None
        };
        let (lhs_span, rhs_span) = (lhs.span, if let Some(ref x) = rhs {
          x.span
        } else {
          cur_op_span
        });
        let limits = if op == AssocOp::DotDot {
          RangeLimits::HalfOpen
        } else {
          RangeLimits::Closed
        };

        let r = self.mk_range(Some(lhs), rhs, limits)?;
        lhs = self.mk_expr(lhs_span.lo, rhs_span.hi, r, ThinVec::new());
        break
      }

      let rhs = match op.fixity() {
        Fixity::Right => self.with_res(
          restrictions - RESTRICTION_STMT_EXPR,
          |this| {
            this.parse_assoc_expr_with(op.precedence(),
                                       LhsExpr::NotYetParsed)
          }),
        Fixity::Left => self.with_res(
          restrictions - RESTRICTION_STMT_EXPR,
          |this| {
            this.parse_assoc_expr_with(op.precedence() + 1,
                                       LhsExpr::NotYetParsed)
          }),
        // We currently have no non-associative operators that are not handled above by
        // the special cases. The code is here only for future convenience.
        Fixity::None => self.with_res(
          restrictions - RESTRICTION_STMT_EXPR,
          |this| {
            this.parse_assoc_expr_with(op.precedence() + 1,
                                       LhsExpr::NotYetParsed)
          }),
      }?;

      let (lo, hi) = (lhs.span.lo, rhs.span.hi);
      lhs = match op {
        AssocOp::Add | AssocOp::Subtract | AssocOp::Multiply | AssocOp::Divide |
        AssocOp::Modulus | AssocOp::LAnd | AssocOp::LOr | AssocOp::BitXor |
        AssocOp::BitAnd | AssocOp::BitOr | AssocOp::ShiftLeft | AssocOp::ShiftRight |
        AssocOp::Equal | AssocOp::Less | AssocOp::LessEqual | AssocOp::NotEqual |
        AssocOp::Greater | AssocOp::GreaterEqual => {
          let ast_op = op.to_ast_binop().unwrap();
          let (lhs_span, rhs_span) = (lhs.span, rhs.span);
          let binary = self.mk_binary(codemap::respan(cur_op_span, ast_op), lhs, rhs);
          self.mk_expr(lhs_span.lo, rhs_span.hi, binary, ThinVec::new())
        }
        AssocOp::Assign =>
          self.mk_expr(lhs.span.lo, rhs.span.hi, ExprKind::Assign(lhs, rhs), ThinVec::new()),
        AssocOp::Inplace =>
          self.mk_expr(lhs.span.lo, rhs.span.hi, ExprKind::InPlace(lhs, rhs), ThinVec::new()),
        AssocOp::AssignOp(k) => {
          let aop = match k {
            token::Plus =>    BinOpKind::Add,
            token::Minus =>   BinOpKind::Sub,
            token::Star =>    BinOpKind::Mul,
            token::Slash =>   BinOpKind::Div,
            token::Percent => BinOpKind::Rem,
            token::Caret =>   BinOpKind::BitXor,
            token::And =>     BinOpKind::BitAnd,
            token::Or =>      BinOpKind::BitOr,
            token::LShift =>  BinOpKind::LShift,
            token::RShift =>  BinOpKind::RShift,
          };
          let (lhs_span, rhs_span) = (lhs.span, rhs.span);
          let aopexpr = self.mk_assign_op(codemap::respan(cur_op_span, aop), lhs, rhs);
          self.mk_expr(lhs_span.lo, rhs_span.hi, aopexpr, ThinVec::new())
        }
        AssocOp::As | AssocOp::Colon | AssocOp::DotDot | AssocOp::DotDotDot => {
          self.bug("As, Colon, DotDot or DotDotDot branch reached")
        }
      };

      if op.fixity() == Fixity::None { break }
    }

    Ok(lhs)
  }

  /// Produce an error if comparison operators are chained (RFC #558).
  /// We only need to check lhs, not rhs, because all comparison ops
  /// have same precedence and are left-associative
  fn check_no_chained_comparison(&mut self, lhs: &Expr, outer_op: &AssocOp) {
    debug_assert!(outer_op.is_comparison());
    match lhs.node {
      ExprKind::Binary(op, _, _) if op.node.is_comparison() => {
        // respan to include both operators
        let op_span = mk_span(op.span.lo, self.span.hi);
        let mut err = self.diagnostic().struct_span_err(op_span,
                                                        "chained comparison operators require parentheses");
        if op.node == BinOpKind::Lt && *outer_op == AssocOp::Greater {
          err.help(
            "use `::<...>` instead of `<...>` if you meant to specify type arguments");
        }
        err.emit();
      }
      _ => {}
    }
  }

  fn is_at_start_of_range_notation_rhs(&self) -> bool {
    if self.token.can_begin_expr() {
      // parse `for i in 1.. { }` as infinite loop, not as `for i in (1..{})`.
      if self.token == token::OpenDelim(token::Brace) {
        return !self.restrictions.contains(RESTRICTION_NO_STRUCT_LITERAL);
      }
      true
    } else {
      false
    }
  }

  /// Parse a prefix-unary-operator expr
  #[allow(unused_variables)]
  pub fn parse_prefix_expr(&mut self,
                           already_parsed_attrs: Option<ThinVec<Attribute>>)
                           -> PResult<'a, P<Expr>> {
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
    Ok(self.mk_expr(lo, hi, ex, ThinVec::new()))
  }

  /// Parse prefix-forms of range notation: `..expr`, `..`, `...expr`
  #[allow(unused_variables)]
  fn parse_prefix_range_expr(&mut self,
                             already_parsed_attrs: Option<ThinVec<Attribute>>)
                             -> PResult<'a, P<Expr>> {
    unimplemented!()
  }

  /// parse a.b, a.1 or a(13) or a[4] or just a
  #[allow(unused_variables)]
  pub fn parse_dot_or_call_expr(&mut self,
                                already_parsed_attrs: Option<ThinVec<Attribute>>)
                                -> PResult<'a, P<Expr>> {
    let lo = self.span.lo;
    let b = self.parse_bottom_expr()?;
    self.parse_dot_or_call_expr_with(b, lo, ThinVec::new())
  }

  pub fn parse_dot_or_call_expr_with(&mut self,
                                     e0: P<Expr>,
                                     lo: BytePos,
                                     mut attrs: ThinVec<Attribute>)
                                     -> PResult<'a, P<Expr>> {
    // Stitch the list of outer attributes onto the return value.
    // A little bit ugly, but the best way given the current code
    // structure
    self.parse_dot_or_call_expr_with_(e0, lo)
    .map(|expr|
         expr.map(|mut expr| {
           attrs.extend::<Vec<_>>(expr.attrs.into());
           expr.attrs = attrs;
           match expr.node {
             ExprKind::If(..) | ExprKind::IfLet(..) => {
               if !expr.attrs.is_empty() {
                 // Just point to the first attribute in there...
                 let span = expr.attrs[0].span;

                 self.span_err(span,
                               "attributes are not yet allowed on `if` \
                               expressions");
               }
             }
             _ => {}
           }
           expr
         })
    )
  }

  /// parse a single token tree from the input.
  pub fn parse_token_tree(&mut self) -> PResult<'a, TokenTree> {
    // FIXME #6994: currently, this is too eager. It
    // parses token trees but also identifies TokenType::Sequence's
    // and token::SubstNt's; it's too early to know yet
    // whether something will be a nonterminal or a seq
    // yet.

    match self.token {
      token::Eof => {
        let mut err: DiagnosticBuilder =
        self.diagnostic().struct_span_err(self.span,
                                          "this file contains an un-closed delimiter");
        for &(_, sp) in &self.open_braces {
          err.span_help(sp, "did you mean to close this delimiter?");
        }

        Err(err)
      },
      token::OpenDelim(delim) => {
        let parsing_token_tree = ::std::mem::replace(&mut self.parsing_token_tree, true);
        // The span for beginning of the delimited section
        let pre_span = self.span;

        // Parse the open delimiter.
        self.open_braces.push((delim, self.span));
        let open_span = self.span;
        self.bump();

        // Parse the token trees within the delimiters.
        // We stop at any delimiter so we can try to recover if the user
        // uses an incorrect delimiter.
        let tts = self.parse_seq_to_before_tokens(&[&token::CloseDelim(token::Brace),
          &token::CloseDelim(token::Paren),
          &token::CloseDelim(token::Bracket)],
                                                  SeqSep::none(),
                                                  |p| p.parse_token_tree(),
                                                  |mut e| e.emit());

        let close_span = self.span;
        // Expand to cover the entire delimited token tree
        let span = Span { hi: close_span.hi, ..pre_span };

        match self.token {
          // Correct delimiter.
          token::CloseDelim(d) if d == delim => {
            self.open_braces.pop().unwrap();

            // Parse the close delimiter.
            self.bump();
          }
          // Incorrect delimiter.
          token::CloseDelim(other) => {
            let token_str = self.this_token_to_string();
            let mut err = self.diagnostic().struct_span_err(self.span,
                                                            &format!("incorrect close delimiter: `{}`", token_str));
            // This is a conservative error: only report the last unclosed delimiter.
            // The previous unclosed delimiters could actually be closed! The parser
            // just hasn't gotten to them yet.
            if let Some(&(_, sp)) = self.open_braces.last() {
              err.span_note(sp, "unclosed delimiter");
            };
            err.emit();

            self.open_braces.pop().unwrap();

            // If the incorrect delimiter matches an earlier opening
            // delimiter, then don't consume it (it can be used to
            // close the earlier one). Otherwise, consume it.
            // E.g., we try to recover from:
            // fn foo() {
            //     bar(baz(
            // }  // Incorrect delimiter but matches the earlier `{`
            if !self.open_braces.iter().any(|&(b, _)| b == other) {
              self.bump();
            }
          }
          token::Eof => {
            // Silently recover, the EOF token will be seen again
            // and an error emitted then. Thus we don't pop from
            // self.open_braces here.
          },
          _ => {}
        }

        self.parsing_token_tree = parsing_token_tree;
        Ok(TokenTree::Delimited(span, Rc::new(Delimited {
          delim: delim,
          open_span: open_span,
          tts: tts,
          close_span: close_span,
        })))
      },
      _ => {
        // invariants: the current token is not a left-delimiter,
        // not an EOF, and not the desired right-delimiter (if
        // it were, parse_seq_to_before_end would have prevented
        // reaching this point).
        match self.token {
          token::CloseDelim(_) => {
            // An unexpected closing delimiter (i.e., there is no
            // matching opening delimiter).
            let token_str = self.this_token_to_string();
            let err = self.diagnostic().struct_span_err(self.span,
                                                        &format!("unexpected close delimiter: `{}`", token_str));
            Err(err)
          },
          _ => {
            Ok(TokenTree::Token(self.span, self.bump_and_get()))
          }
        }
      }
    }
  }

  // parse a stream of tokens into a list of TokenTree's,
  // up to EOF.
  pub fn parse_all_token_trees(&mut self) -> PResult<'a, Vec<TokenTree>> {
    let mut tts = Vec::new();
    while self.token != token::Eof {
      tts.push(self.parse_token_tree()?);
    }
    Ok(tts)
  }

  // Assuming we have just parsed `.foo` (i.e., a dot and an ident), continue
  // parsing into an expression.
  fn parse_dot_suffix(&mut self,
                      ident: ast::Ident,
                      ident_span: Span,
                      self_value: P<Expr>,
                      lo: BytePos)
                      -> PResult<'a, P<Expr>> {
    Ok(match self.token {
      // expr.f() method call.
      token::OpenDelim(token::Paren) => {
        let mut es = self.parse_unspanned_seq(
          &token::OpenDelim(token::Paren),
          &token::CloseDelim(token::Paren),
          SeqSep::trailing_allowed(token::Comma),
          |p| Ok(p.parse_expr()?)
        )?;
        let hi = self.last_span.hi;

        es.insert(0, self_value);
        let id = spanned(ident_span.lo, ident_span.hi, ident);
        let nd = self.mk_method_call(id, Vec::new(), es);
        self.mk_expr(lo, hi, nd, ThinVec::new())
      }
      // Field access.
      _ => {
        let id = spanned(ident_span.lo, ident_span.hi, ident);
        let field = self.mk_field(self_value, id);
        self.mk_expr(lo, ident_span.hi, field, ThinVec::new())
      }
    })
  }

  fn parse_dot_or_call_expr_with_(&mut self, e0: P<Expr>, lo: BytePos) -> PResult<'a, P<Expr>> {
    let mut e = e0;
    let mut hi;

    loop {
      // expr.f
      if self.eat(&token::Dot) {
        match self.token {
          token::Ident(i) => {
            let dot_pos = self.last_span.hi;
            hi = self.span.hi;
            self.bump();

            e = self.parse_dot_suffix(i, mk_span(dot_pos, hi), e, lo)?;
          }
          token::Literal(token::Integer(n), suf) => {
            let sp = self.span;

            // A tuple index may not have a suffix
            self.expect_no_suffix(sp, "tuple index", suf);

            let dot = self.last_span.hi;
            hi = self.span.hi;
            self.bump();

            let index = n.as_str().parse::<usize>().ok();
            match index {
              Some(n) => {
                let id = spanned(dot, hi, n);
                let field = self.mk_tup_field(e, id);
                e = self.mk_expr(lo, hi, field, ThinVec::new());
              }
              None => {
                let last_span = self.last_span;
                self.span_err(last_span, "invalid tuple or tuple struct index");
              }
            }
          }
          token::Literal(token::Float(n), _suf) => {
            self.bump();
            let last_span = self.last_span;
            let fstr = n.as_str();
            let mut err = self.diagnostic().struct_span_err(last_span,
                                                            &format!("unexpected token: `{}`", n.as_str()));
            if fstr.chars().all(|x| "0123456789.".contains(x)) {
              let float = match fstr.parse::<f64>().ok() {
                Some(f) => f,
                None => continue,
              };
              err.help(&format!("try parenthesizing the first index; e.g., `(foo.{}){}`",
              float.trunc() as usize,
              format!(".{}", fstr.splitn(2, ".").last().unwrap())));
            }
            return Err(err);

          }
          _ => {
            // FIXME Could factor this out into non_fatal_unexpected or something.
            let actual = self.this_token_to_string();
            self.span_err(self.span, &format!("unexpected token: `{}`", actual));

            let dot_pos = self.last_span.hi;
            e = self.parse_dot_suffix(keywords::Invalid.ident(),
                                      mk_span(dot_pos, dot_pos),
                                      e, lo)?;
          }
        }

        continue;
      }
      if self.expr_is_complete(&e) { break; }
      match self.token {
        // expr(...)
        token::OpenDelim(token::Paren) => {
          let es = self.parse_unspanned_seq(
            &token::OpenDelim(token::Paren),
            &token::CloseDelim(token::Paren),
            SeqSep::trailing_allowed(token::Comma),
            |p| Ok(p.parse_expr()?)
          )?;
          hi = self.last_span.hi;

          let nd = self.mk_call(e, es);
          e = self.mk_expr(lo, hi, nd, ThinVec::new());
        }

        // expr[...]
        // Could be either an index expression or a slicing expression.
        token::OpenDelim(token::Bracket) => {
          self.bump();
          let ix = self.parse_expr()?;
          hi = self.span.hi;
          self.commit_expr_expecting(&ix, token::CloseDelim(token::Bracket))?;
          let index = self.mk_index(e, ix);
          e = self.mk_expr(lo, hi, index, ThinVec::new())
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
  fn parse_bottom_expr(&mut self) -> PResult<'a, P<Expr>> {

    // Outer attributes are already parsed and will be
    // added to the return value after the fact.
    //
    // Therefore, prevent sub-parser from parsing
    // attributes by giving them a empty "already parsed" list.
    let mut attrs = ThinVec::new();

    let lo = self.span.lo;
    let mut hi = self.span.hi;

    let ex: ExprKind;

    // Note: when adding new syntax here, don't forget to adjust Token::can_begin_expr().
    match self.token {
      token::OpenDelim(token::Paren) => {
        self.bump();

        // (e) is parenthesized e
        // (e,) is a tuple with only one field, e
        let mut es = vec![];
        let mut trailing_comma = false;
        while self.token != token::CloseDelim(token::Paren) {
          es.push(self.parse_expr()?);
          self.commit_expr(&es.last().unwrap(), &[],
                           &[token::Comma, token::CloseDelim(token::Paren)])?;
          if self.check(&token::Comma) {
            trailing_comma = true;

            self.bump();
          } else {
            trailing_comma = false;
            break;
          }
        }
        self.bump();

        hi = self.last_span.hi;
        return if es.len() == 1 && !trailing_comma {
          Ok(self.mk_expr(lo, hi, ExprKind::Paren(es.into_iter().nth(0).unwrap()), ThinVec::new()))
        } else {
          Ok(self.mk_expr(lo, hi, ExprKind::Tup(es), ThinVec::new()))
        }
      }
      token::OpenDelim(token::Brace) => {
        return self.parse_block_expr(lo, BlockCheckMode::Default, attrs);
      }
      token::BinOp(token::Or) |  token::OrOr => {
        let lo = self.span.lo;
        return self.parse_lambda_expr(lo, CaptureBy::Ref, attrs);
      }
      token::OpenDelim(token::Bracket) => {
        self.bump();

        attrs.extend(self.parse_inner_attributes()?);

        if self.check(&token::CloseDelim(token::Bracket)) {
          // Empty vector.
          self.bump();
          ex = ExprKind::Vec(Vec::new());
        } else {
          // Nonempty vector.
          let first_expr = self.parse_expr()?;
          if self.check(&token::SemiColon) {
            // Repeating array syntax: [ 0; 512 ]
            self.bump();
            let count = self.parse_expr()?;
            self.expect(&token::CloseDelim(token::Bracket))?;
            ex = ExprKind::Repeat(first_expr, count);
          } else if self.check(&token::Comma) {
            // Vector with two or more elements.
            self.bump();
            let remaining_exprs = self.parse_seq_to_end(
              &token::CloseDelim(token::Bracket),
              SeqSep::trailing_allowed(token::Comma),
              |p| Ok(p.parse_expr()?)
            )?;
            let mut exprs = vec!(first_expr);
            exprs.extend(remaining_exprs);
            ex = ExprKind::Vec(exprs);
          } else {
            // Vector with one element.
            self.expect(&token::CloseDelim(token::Bracket))?;
            ex = ExprKind::Vec(vec!(first_expr));
          }
        }
        hi = self.last_span.hi;
      }
      _ => {
        if self.eat_keyword(keywords::If) {
          return self.parse_if_expr(attrs);
        }
        if self.eat_keyword(keywords::For) {
          let lo = self.last_span.lo;
          return self.parse_for_expr(None, lo, attrs);
        }
        if self.eat_keyword(keywords::While) {
          let lo = self.last_span.lo;
          return self.parse_while_expr(None, lo, attrs);
        }
        if self.eat_keyword(keywords::Loop) {
          let lo = self.last_span.lo;
          return self.parse_loop_expr(None, lo, attrs);
        }
        if self.eat_keyword(keywords::Continue) {
          let ex = if self.token.is_ident() {
            ExprKind::Continue(Some(Spanned{
              node: self.parse_ident()?,
              span: self.span
            }))
          } else {
            ExprKind::Continue(None)
          };
          hi = self.last_span.hi;
          return Ok(self.mk_expr(lo, hi, ex, ThinVec::new()));
        }
        if self.eat_keyword(keywords::Match) {
          return self.parse_match_expr(attrs);
        }
        if self.eat_keyword(keywords::Unsafe) {
          return self.parse_block_expr(
            lo,
            BlockCheckMode::Unsafe(UnsafeSource::UserProvided),
            attrs);
        }
        if self.eat_keyword(keywords::Return) {
          if self.token.can_begin_expr() {
            let e = self.parse_expr()?;
            hi = e.span.hi;
            ex = ExprKind::Ret(Some(e));
          } else {
            ex = ExprKind::Ret(None);
          }
        } else if self.eat_keyword(keywords::Break) {
          ex = if self.token.is_ident() {
            ExprKind::Break(Some(Spanned {
            node: self.parse_ident()?,
            span: self.span
            }))
          } else {
            ExprKind::Break(None)
          };
          hi = self.last_span.hi;
        } else if self.token.is_keyword(keywords::Let) {
          // Catch this syntax error here, instead of in `check_strict_keywords`, so
          // that we can explicitly mention that let is not to be used as an expression
          let mut db = self.fatal("expected expression, found statement (`let`)");
          db.note("variable declaration using `let` is a statement");
          return Err(db);
        } else if self.token.is_keyword(keywords::Var) {
          // Catch this syntax error here, instead of in `check_strict_keywords`, so
          // that we can explicitly mention that let is not to be used as an expression
          let mut db = self.fatal("expected expression, found statement (`var`)");
          db.note("variable declaration using `var` is a statement");
          return Err(db);
        } else if self.token.is_path_start() {
          let path = self.parse_path()?;

          if self.check(&token::OpenDelim(token::Brace)) {
            // This is a struct literal, unless we're prohibited
            // from parsing struct literals here.
            let prohibited = self.restrictions.contains(RESTRICTION_NO_STRUCT_LITERAL);

            if !prohibited {
              // It's a struct literal.
              self.bump();
              let mut fields = Vec::new();
              let mut base = None;

              while self.token != token::CloseDelim(token::Brace) {
                if self.eat(&token::DotDot) {
                  match self.parse_expr() {
                    Ok(e) => {
                      base = Some(e);
                    }
                    Err(mut e) => {
                      e.emit();
                      self.recover_stmt();
                    }
                  }
                  break;
                }

                match self.parse_field() {
                  Ok(f) => fields.push(f),
                  Err(mut e) => {
                    e.emit();
                    self.recover_stmt();
                    break;
                  }
                }

                match self.commit_expr(&fields.last().unwrap().expr,
                                       &[token::Comma],
                                       &[token::CloseDelim(token::Brace)]) {
                  Ok(()) => {}
                  Err(mut e) => {
                    e.emit();
                    self.recover_stmt();
                    break;
                  }
                }
              }

              hi = self.span.hi;
              self.expect(&token::CloseDelim(token::Brace))?;
              ex = ExprKind::Struct(path, fields, base);
              return Ok(self.mk_expr(lo, hi, ex, ThinVec::new()));
            }
          }

          hi = path.span.hi;
          ex = ExprKind::Path(None, path);
        } else {
          debug!("reach parse_lit");
          match self.parse_lit() {
            Ok(lit) => {
              hi = lit.span.hi;
              ex = ExprKind::Lit(P(lit));
            }
            Err(mut err) => {
              err.cancel();
              let msg = format!("expected expression, found {}",
              self.this_token_descr());
              return Err(self.fatal(&msg));
            }
          }
        }
      }
    }

    return Ok(self.mk_expr(lo, hi, ex, ThinVec::new()));
  }

  // `move |args| expr`
  pub fn parse_lambda_expr(&mut self,
                           lo: BytePos,
                           capture_clause: CaptureBy,
                           attrs: ThinVec<Attribute>)
                           -> PResult<'a, P<Expr>>
  {
    let decl = self.parse_fn_block_decl()?;
    let decl_hi = self.last_span.hi;
    let body = match decl.output {
      FunctionRetTy::Default(_) => {
        // If no explicit return type is given, parse any
        // expr and wrap it up in a dummy block:
        let body_expr = self.parse_expr()?;
        P(ast::Block {
          id: ast::DUMMY_NODE_ID,
          span: body_expr.span,
          stmts: vec![Stmt {
                        span: body_expr.span,
                        node: StmtKind::Expr(body_expr),
                        id: ast::DUMMY_NODE_ID,
                    }],
          rules: BlockCheckMode::Default,
        })
      }
      _ => {
        // If an explicit return type is given, require a
        // block to appear (RFC 968).
        self.parse_block()?
      }
    };

    Ok(self.mk_expr(
      lo,
      body.span.hi,
      ExprKind::Closure(capture_clause, decl, body, codespan::mk_span(lo, decl_hi)),
      attrs))
  }

  // parse the |arg, arg| header on a lambda
  fn parse_fn_block_decl(&mut self) -> PResult<'a, P<FnDecl>> {
    let inputs_captures = {
      if self.eat(&token::OrOr) {
        Vec::new()
      } else {
        self.expect(&token::BinOp(token::Or))?;
        let args = self.parse_seq_to_before_end(
          &token::BinOp(token::Or),
          SeqSep::trailing_allowed(token::Comma),
          |p| p.parse_fn_block_arg()
        );
        self.bump();
        args
      }
    };
    let output = self.parse_ret_ty()?;

    Ok(P(FnDecl {
      inputs: inputs_captures,
      output: output,
      variadic: false
    }))
  }

  /// Parse an argument in a lambda header e.g. |arg, arg|
  pub fn parse_fn_block_arg(&mut self) -> PResult<'a, Arg> {
    let pat = self.parse_pat()?;
    let t = if self.eat(&token::Colon) {
      self.parse_ty()?
    } else {
      P(Ty {
        id: ast::DUMMY_NODE_ID,
        node: TyKind::Infer,
        span: codespan::mk_span(self.span.lo, self.span.hi),
      })
    };
    Ok(Arg {
      ty: t,
      pat: pat,
      id: ast::DUMMY_NODE_ID
    })
  }

  /// Parse a block or unsafe block
  pub fn parse_block_expr(&mut self, lo: BytePos, blk_mode: BlockCheckMode,
                          outer_attrs: ThinVec<Attribute>)
                          -> PResult<'a, P<Expr>> {

    self.expect(&token::OpenDelim(token::Brace))?;

    let mut attrs = outer_attrs;
    attrs.extend(self.parse_inner_attributes()?);

    let blk = self.parse_block_tail(lo, blk_mode)?;
    return Ok(self.mk_expr(blk.span.lo, blk.span.hi, ExprKind::Block(blk), attrs));
  }

  /// Parse an 'if' or 'if let' expression ('if' token already eaten)
  pub fn parse_if_expr(&mut self, attrs: ThinVec<Attribute>) -> PResult<'a, P<Expr>> {
    if self.check_keyword(keywords::Let) {
      return self.parse_if_let_expr(attrs);
    }
    let lo = self.last_span.lo;
    let cond = self.parse_expr_res(RESTRICTION_NO_STRUCT_LITERAL, None)?;
    let thn = self.parse_block()?;
    let mut els: Option<P<Expr>> = None;
    let mut hi = thn.span.hi;
    if self.eat_keyword(keywords::Else) {
      let elexpr = self.parse_else_expr()?;
      hi = elexpr.span.hi;
      els = Some(elexpr);
    }
    Ok(self.mk_expr(lo, hi, ExprKind::If(cond, thn, els), attrs))
  }

  /// Parse an 'if let' expression ('if' token already eaten)
  pub fn parse_if_let_expr(&mut self, attrs: ThinVec<Attribute>)
      -> PResult<'a, P<Expr>> {
    let lo = self.last_span.lo;
    self.expect_keyword(keywords::Let)?;
    let pat = self.parse_pat()?;
    self.expect(&token::Eq)?;
    let expr = self.parse_expr_res(RESTRICTION_NO_STRUCT_LITERAL, None)?;
    let thn = self.parse_block()?;
    let (hi, els) = if self.eat_keyword(keywords::Else) {
      let expr = self.parse_else_expr()?;
      (expr.span.hi, Some(expr))
    } else {
      (thn.span.hi, None)
    };
    Ok(self.mk_expr(lo, hi, ExprKind::IfLet(pat, expr, thn, els), attrs))
  }

  // `else` token already eaten
  pub fn parse_else_expr(&mut self) -> PResult<'a, P<Expr>> {
    if self.eat_keyword(keywords::If) {
      return self.parse_if_expr(ThinVec::new());
    } else {
      let blk = self.parse_block()?;
      return Ok(self.mk_expr(blk.span.lo, blk.span.hi, ExprKind::Block(blk), ThinVec::new()));
    }
  }

  /// Parse a 'for' .. 'in' expression ('for' token already eaten)
  pub fn parse_for_expr(&mut self, opt_ident: Option<ast::SpannedIdent>,
                        span_lo: BytePos,
                        mut attrs: ThinVec<Attribute>) -> PResult<'a, P<Expr>> {
    // Parse: `for <src_pat> in <src_expr> <src_loop_block>`

    let pat = self.parse_pat()?;
    self.expect_keyword(keywords::In)?;
    let expr = self.parse_expr_res(RESTRICTION_NO_STRUCT_LITERAL, None)?;
    let (iattrs, loop_block) = self.parse_inner_attrs_and_block()?;
    attrs.extend(iattrs);

    let hi = self.last_span.hi;

    Ok(self.mk_expr(span_lo, hi,
                    ExprKind::ForLoop(pat, expr, loop_block, opt_ident),
                    attrs))
  }

  /// Parse a 'while' or 'while let' expression ('while' token already eaten)
  pub fn parse_while_expr(&mut self, opt_ident: Option<ast::SpannedIdent>,
                          span_lo: BytePos,
                          mut attrs: ThinVec<Attribute>) -> PResult<'a, P<Expr>> {
    if self.token.is_keyword(keywords::Let) {
      return self.parse_while_let_expr(opt_ident, span_lo, attrs);
    }
    let cond = self.parse_expr_res(RESTRICTION_NO_STRUCT_LITERAL, None)?;
    let (iattrs, body) = self.parse_inner_attrs_and_block()?;
    attrs.extend(iattrs);
    let hi = body.span.hi;
    return Ok(self.mk_expr(span_lo, hi, ExprKind::While(cond, body, opt_ident),
                           attrs));
  }

  /// Parse a 'while let' expression ('while' token already eaten)
  pub fn parse_while_let_expr(&mut self, opt_ident: Option<ast::SpannedIdent>,
                              span_lo: BytePos,
                              mut attrs: ThinVec<Attribute>) -> PResult<'a, P<Expr>> {
    self.expect_keyword(keywords::Let)?;
    let pat = self.parse_pat()?;
    self.expect(&token::Eq)?;
    let expr = self.parse_expr_res(RESTRICTION_NO_STRUCT_LITERAL, None)?;
    let (iattrs, body) = self.parse_inner_attrs_and_block()?;
    attrs.extend(iattrs);
    let hi = body.span.hi;
    return Ok(self.mk_expr(span_lo, hi, ExprKind::WhileLet(pat, expr, body, opt_ident), attrs));
  }

  // parse `loop {...}`, `loop` token already eaten
  pub fn parse_loop_expr(&mut self, opt_ident: Option<ast::SpannedIdent>,
                         span_lo: BytePos,
                         mut attrs: ThinVec<Attribute>) -> PResult<'a, P<Expr>> {
    let (iattrs, body) = self.parse_inner_attrs_and_block()?;
    attrs.extend(iattrs);
    let hi = body.span.hi;
    Ok(self.mk_expr(span_lo, hi, ExprKind::Loop(body, opt_ident), attrs))
  }

  // `match` token already eaten
  fn parse_match_expr(&mut self, mut attrs: ThinVec<Attribute>) -> PResult<'a, P<Expr>> {
    let match_span = self.last_span;
    let lo = self.last_span.lo;
    let discriminant = self.parse_expr_res(RESTRICTION_NO_STRUCT_LITERAL, None)?;
    if let Err(mut e) = self.expect(&token::OpenDelim(token::Brace)) {
      if self.token == token::Token::SemiColon {
        e.span_note(match_span, "did you mean to remove this `match` keyword?");
      }
      return Err(e)
    }
    attrs.extend(self.parse_inner_attributes()?);

    let mut arms: Vec<Arm> = Vec::new();
    while self.token != token::CloseDelim(token::Brace) {
      match self.parse_arm() {
        Ok(arm) => arms.push(arm),
        Err(mut e) => {
          // Recover by skipping to the end of the block.
          e.emit();
          self.recover_stmt();
          let hi = self.span.hi;
          if self.token == token::CloseDelim(token::Brace) {
            self.bump();
          }
          return Ok(self.mk_expr(lo, hi, ExprKind::Match(discriminant, arms), attrs));
        }
      }
    }
    let hi = self.span.hi;
    self.bump();
    return Ok(self.mk_expr(lo, hi, ExprKind::Match(discriminant, arms), attrs));
  }

  pub fn parse_arm(&mut self) -> PResult<'a, Arm> {
    //let attrs = self.parse_outer_attributes()?;
    let attrs = Vec::new();
    let pats = self.parse_pats()?;
    let mut guard = None;
    if self.eat_keyword(keywords::If) {
      guard = Some(self.parse_expr()?);
    }
    self.expect(&token::FatArrow)?;
    let expr = self.parse_expr_res(RESTRICTION_STMT_EXPR, None)?;

    let require_comma =
      !expr_is_simple_block(&expr) && self.token != token::CloseDelim(token::Brace);

    if require_comma {
      self.expect_one_of(&[token::Comma], &[token::CloseDelim(token::Brace)])?;
    } else {
      self.eat(&token::Comma);
    }

    Ok(ast::Arm {
      attrs: attrs,
      pats: pats,
      guard: guard,
      body: expr,
    })
  }

  /// Parse patterns, separated by '|' s
  fn parse_pats(&mut self) -> PResult<'a, Vec<P<Pat>>> {
    let mut pats = Vec::new();
    loop {
      pats.push(self.parse_pat()?);
      if self.check(&token::BinOp(token::Or)) { self.bump();}
        else { return Ok(pats); }
    };
  }

  /// Matches lit = true | false | token_lit
  pub fn parse_lit(&mut self) -> PResult<'a, Lit> {
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
  pub fn parse_lit_token(&mut self) -> PResult<'a, LitKind> {
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

  /// Parse a type.
  pub fn parse_ty(&mut self) -> PResult<'a, P<Ty>> {

    let lo = self.span.lo;

    let t = if self.check(&token::OpenDelim(token::Paren)) {
      self.bump();

      // (t) is a parenthesized ty
      // (t,) is the type of a tuple with only one field,
      // of type t
      let mut ts = vec![];
      let mut last_comma = false;
      while self.token != token::CloseDelim(token::Paren) {
        ts.push(self.parse_ty()?);
        if self.check(&token::Comma) {
          last_comma = true;
          self.bump();
        } else {
          last_comma = false;
          break;
        }
      }

      self.expect(&token::CloseDelim(token::Paren))?;
      if ts.len() == 1 && !last_comma {
        TyKind::Paren(ts.into_iter().nth(0).unwrap())
      } else {
        TyKind::Tup(ts)
      }
    } else if self.token.is_path_start() {
      let path = self.parse_path()?;
      // NAMED TYPE
      TyKind::Path(path)
    } else if self.eat(&token::Underscore) {
      // TYPE TO BE INFERRED
      TyKind::Infer
    } else {
      let msg = format!("expected type, found {}", self.this_token_descr());
      return Err(self.fatal(&msg));
    };

    let sp = mk_span(lo, self.last_span.hi);
    Ok(P(Ty {id: ast::DUMMY_NODE_ID, node: t, span: sp}))
  }

  /// Parses a path and optional type parameter bounds, depending on the
  /// mode. The `mode` parameter determines whether lifetimes, types, and/or
  /// bounds are permitted and whether `::` must precede type parameter
  /// groups.
  pub fn parse_path(&mut self) -> PResult<'a, ast::Path> {

    let lo = self.span.lo;
    let is_global = self.eat(&token::ModSep);


    let segments = self.parse_path_segments_without_types()?;

    // Assemble the span.
    let span = mk_span(lo, self.last_span.hi);

    // Assemble the result.
    Ok(ast::Path {
      span: span,
      global: is_global,
      segments: segments,
    })
  }

  /// Examples:
  /// - `a::b::c`
  pub fn parse_path_segments_without_types(&mut self) -> PResult<'a, Vec<ast::PathSegment>> {
    let mut segments = Vec::new();
    loop {
      // First, parse an identifier.
      let identifier = self.parse_path_segment_ident()?;

      // Assemble and push the result.
      segments.push(ast::PathSegment { identifier: identifier });

      // If we do not see a `::` or see `::{`/`::*`, stop.
      if !self.check(&token::ModSep) || self.is_import_coupler() {
        return Ok(segments);
      } else {
        self.bump();
      }
    }
  }

  /// `::{` or `::*`
  fn is_import_coupler(&mut self) -> bool {
    self.check(&token::ModSep) &&
      self.look_ahead(1, |t| *t == token::OpenDelim(token::Brace) ||
    *t == token::BinOp(token::Star))
  }

  pub fn parse_path_segment_ident(&mut self) -> PResult<'a, ast::Ident> {
    match self.token {
      token::Ident(sid) if self.token.is_path_segment_keyword() => {
        self.bump();
        Ok(sid)
      }
      _ => self.parse_ident(),
    }
  }

  pub fn parse_ident(&mut self) -> PResult<'a, ast::Ident> {
    self.check_strict_keywords();
    self.check_reserved_keywords();
    match self.token {
      token::Ident(i) => {
        self.bump();
        Ok(i)
      }
      _ => {
        let mut err = self.fatal(&format!("expected identifier, found `{}`",
                                 self.this_token_to_string()));
        if self.token == token::Underscore {
          err.note("`_` is a wildcard pattern, not an identifier");
        }
        Err(err)
      }
    }
  }

  /// Parse ident COLON expr
  pub fn parse_field(&mut self) -> PResult<'a, Field> {
    let lo = self.span.lo;
    let i = self.parse_ident()?;
    let hi = self.last_span.hi;
    self.expect(&token::Colon)?;
    let e = self.parse_expr()?;
    Ok(ast::Field {
      ident: spanned(lo, hi, i),
      span: mk_span(lo, e.span.hi),
      expr: e,
    })
  }

  /// Parse a pattern.
  pub fn parse_pat(&mut self) -> PResult<'a, P<Pat>> {
    let lo = self.span.lo;
    let pat;

    match self.token {
      token::Underscore => {
        // Parse _
        self.bump();
        pat = PatKind::Wild;
      }
      token::OpenDelim(token::Paren) => {
        // Parse (pat,pat,pat,...) as tuple pattern
        self.bump();
        let (fields, ddpos) = self.parse_pat_tuple_elements(false)?;
        self.expect(&token::CloseDelim(token::Paren))?;
        pat = PatKind::Tuple(fields, ddpos);
      }
      token::OpenDelim(token::Bracket) => {
        // Parse [pat,pat,...] as slice pattern
        self.bump();
        let (before, slice, after) = self.parse_pat_vec_elements()?;
        self.expect(&token::CloseDelim(token::Bracket))?;
        pat = PatKind::Vec(before, slice, after);
      }
      _ => {
        if self.token.is_path_start() {
          if self.token.is_ident() && self.look_ahead(1, |t|
            *t != token::DotDotDot && *t != token::ModSep) {

            // Parse ident @ pat
            pat = self.parse_pat_ident(BindingMode::ByValue)?;
          } else {
            let qself = None;
            let path = self.parse_path()?;

            match self.token {
              token::DotDotDot => {
                // Parse range
                let hi = self.last_span.hi;
                let begin = self.mk_expr(lo, hi, ExprKind::Path(qself, path), ThinVec::new());
                self.bump();
                let end = self.parse_pat_range_end()?;
                pat = PatKind::Range(begin, end);
              }
              token::OpenDelim(token::Brace) => {
                // Parse struct pattern
                self.bump();
                let (fields, etc) = self.parse_pat_fields().unwrap_or_else(|mut e| {
                  e.emit();
                  self.recover_stmt();
                  (vec![], false)
                });
                self.bump();
                pat = PatKind::Struct(path, fields, etc);
              }
              token::OpenDelim(token::Paren) => {
                if qself.is_some() {
                  return Err(self.fatal("unexpected `(` after qualified path"));
                }
                // Parse tuple struct or enum pattern
                self.bump();
                let (fields, ddpos) = self.parse_pat_tuple_elements(false)?;
                self.expect(&token::CloseDelim(token::Paren))?;
                pat = PatKind::TupleStruct(path, fields, ddpos)
              }
              _ => {
                pat = match qself {
                  // Parse qualified path
                  Some(qself) => PatKind::QPath(qself, path),
                  // Parse nullary enum
                  None => PatKind::Path(path)
                };
              }
            }
          }
        } else {
          let msg = format!("expected type, found {}", self.this_token_descr());
          return Err(self.fatal(&msg));
        }
      }
    }

    let hi = self.last_span.hi;
    Ok(P(ast::Pat {
      id: ast::DUMMY_NODE_ID,
      node: pat,
      span: mk_span(lo, hi),
    }))
  }

  fn parse_pat_tuple_elements(&mut self, unary_needs_comma: bool)
      -> PResult<'a, (Vec<P<Pat>>, Option<usize>)> {

    let mut fields = vec![];
    let mut ddpos = None;

    while !self.check(&token::CloseDelim(token::Paren)) {
      if ddpos.is_none() && self.eat(&token::DotDot) {
        ddpos = Some(fields.len());
        if self.eat(&token::Comma) {
          // `..` needs to be followed by `)` or `, pat`, `..,)` is disallowed.
          fields.push(self.parse_pat()?);
        }
      } else if ddpos.is_some() && self.eat(&token::DotDot) {
        // Emit a friendly error, ignore `..` and continue parsing
        self.span_err(self.last_span, "`..` can only be used once per \
                                               tuple or tuple struct pattern");
      } else {
        fields.push(self.parse_pat()?);
      }

      if !self.check(&token::CloseDelim(token::Paren)) ||
          (unary_needs_comma && fields.len() == 1 && ddpos.is_none()) {
        self.expect(&token::Comma)?;
      }
    }

    Ok((fields, ddpos))
  }

  fn parse_pat_vec_elements(&mut self) -> PResult<'a, (Vec<P<Pat>>, Option<P<Pat>>, Vec<P<Pat>>)> {
    unimplemented!()
  }

  /// Parse the fields of a struct-like pattern
  fn parse_pat_fields(&mut self) -> PResult<'a, (Vec<codemap::Spanned<ast::FieldPat>> , bool)> {
    unimplemented!()
  }

  fn parse_pat_range_end(&mut self) -> PResult<'a, P<Expr>> {
    if self.token.is_path_start() {
      let lo = self.span.lo;
      // Parse an unqualified path
      let (qself, path) = (None, self.parse_path()?);
      let hi = self.last_span.hi;
      Ok(self.mk_expr(lo, hi, ExprKind::Path(qself, path), ThinVec::new()))
    } else {
      self.parse_pat_literal_maybe_minus()
    }
  }

  /// matches '-' lit | lit
  pub fn parse_pat_literal_maybe_minus(&mut self) -> PResult<'a, P<Expr>> {
    let minus_lo = self.span.lo;
    let minus_present = self.eat(&token::BinOp(token::Minus));
    let lo = self.span.lo;
    let literal = P(self.parse_lit()?);
    let hi = self.last_span.hi;
    let expr = self.mk_expr(lo, hi, ExprKind::Lit(literal), ThinVec::new());

    if minus_present {
      let minus_hi = self.last_span.hi;
      let unary = self.mk_unary(UnOp::Neg, expr);
      Ok(self.mk_expr(minus_lo, minus_hi, unary, ThinVec::new()))
    } else {
      Ok(expr)
    }
  }

  /// Parse a sequence parameter of enum variant. For consistency purposes,
  /// these should not be empty.
  pub fn parse_enum_variant_seq<T, F>(&mut self,
                                      bra: &token::Token,
                                      ket: &token::Token,
                                      sep: SeqSep,
                                      f: F)
                                      -> PResult<'a, Vec<T>> where
    F: FnMut(&mut Parser<'a>) -> PResult<'a, T>,
  {
    let result = self.parse_unspanned_seq(bra, ket, sep, f)?;
    if result.is_empty() {
      let last_span = self.last_span;
      self.span_err(last_span,
                    "nullary enum variants are written with no trailing `( )`");
    }
    Ok(result)
  }

  /// Parse ident or ident @ pat
  /// used by the copy foo and ref foo patterns to give a good
  /// error message when parsing mistakes like ref foo(a,b)
  fn parse_pat_ident(&mut self,
                     binding_mode: ast::BindingMode)
                     -> PResult<'a, PatKind> {

    let ident = self.parse_ident()?;
    let last_span = self.last_span;
    let name = codemap::Spanned{span: last_span, node: ident};
    let sub = if self.eat(&token::At) {
      Some(self.parse_pat()?)
    } else {
      None
    };

    // just to be friendly, if they write something like
    //   ref Some(i)
    // we end up here with ( as the current token.  This shortly
    // leads to a parse error.  Note that if there is no explicit
    // binding mode then we do not end up here, because the lookahead
    // will direct us over to parse_enum_variant()
    if self.token == token::OpenDelim(token::Paren) {
      let last_span = self.last_span;
      return Err(self.span_fatal(
        last_span,
        "expected identifier, found enum pattern"))
    }

    Ok(PatKind::Ident(binding_mode, name, sub))
  }

  //-------------------------------------------------------------------------
  // AST Builder API Section
  //-------------------------------------------------------------------------

  fn mk_item(&mut self, lo: BytePos, hi: BytePos, ident: Ident,
    node: ItemKind, vis: Visibility,
    attrs: Vec<Attribute>) -> P<Item> {
    P(Item {
      ident: ident,
      attrs: attrs,
      id: ast::DUMMY_NODE_ID,
      node: node,
      vis: vis,
      span: mk_span(lo, hi)
    })
  }

  pub fn mk_expr(&mut self, lo: BytePos, hi: BytePos,
                 node: ExprKind, attrs: ThinVec<Attribute>) -> P<Expr> {
    P(Expr {
      id: ast::DUMMY_NODE_ID,
      node: node,
      span: mk_span(lo, hi),
      attrs: attrs.into(),
    })
  }

  pub fn mk_unary(&mut self, unop: ast::UnOp, expr: P<Expr>) -> ast::ExprKind {
    ExprKind::Unary(unop, expr)
  }

  pub fn mk_binary(&mut self, binop: ast::BinOp, lhs: P<Expr>, rhs: P<Expr>) -> ast::ExprKind {
    ExprKind::Binary(binop, lhs, rhs)
  }

  pub fn mk_assign_op(&mut self, binop: ast::BinOp, lhs: P<Expr>, rhs: P<Expr>) -> ast::ExprKind {
    ExprKind::AssignOp(binop, lhs, rhs)
  }

  pub fn mk_range(&mut self,
    start: Option<P<Expr>>,
    end: Option<P<Expr>>,
    limits: RangeLimits)
    -> PResult<'a, ast::ExprKind> {
    if end.is_none() && limits == RangeLimits::Closed {
      Err(self.span_fatal_help(self.span,
                               "inclusive range with no end",
                               "inclusive ranges must be bounded at the end \
                                (`...b` or `a...b`)"))
    } else {
      Ok(ExprKind::Range(start, end, limits))
    }
  }

  pub fn mk_field(&mut self, expr: P<Expr>, ident: ast::SpannedIdent) -> ast::ExprKind {
    ExprKind::Field(expr, ident)
  }

  pub fn mk_tup_field(&mut self, expr: P<Expr>, idx: codemap::Spanned<usize>) -> ast::ExprKind {
    ExprKind::TupField(expr, idx)
  }

  pub fn mk_index(&mut self, expr: P<Expr>, idx: P<Expr>) -> ast::ExprKind {
    ExprKind::Index(expr, idx)
  }

  pub fn mk_call(&mut self, f: P<Expr>, args: Vec<P<Expr>>) -> ast::ExprKind {
    ExprKind::Call(f, args)
  }

  fn mk_method_call(&mut self,
                    ident: ast::SpannedIdent,
                    tps: Vec<P<Ty>>,
                    args: Vec<P<Expr>>)
                    -> ast::ExprKind {
    ExprKind::MethodCall(ident, tps, args)
  }
}

fn is_ident_or_underscore(t: &token::Token) -> bool {
  t.is_ident() || *t == token::Underscore
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
    ast::ExprKind::If(..) |
    ast::ExprKind::IfLet(..) |
    ast::ExprKind::Match(..) |
    ast::ExprKind::Block(..) |
    ast::ExprKind::While(..) |
    ast::ExprKind::Loop(..) |
    ast::ExprKind::ForLoop(..) => false,
    _ => true,
  }
}

pub fn expr_is_simple_block(e: &ast::Expr) -> bool {
  match e.node {
    ast::ExprKind::Block(ref block) => block.rules == BlockCheckMode::Default,
    _ => false,
  }
}

/// this statement requires a semicolon after it.
/// note that in one case (stmt_semi), we've already
/// seen the semicolon, and thus don't need another.
pub fn stmt_ends_with_semi(stmt: &ast::StmtKind) -> bool {
  match *stmt {
    ast::StmtKind::Local(_) => true,
    ast::StmtKind::Item(_) => false,
    ast::StmtKind::Expr(ref e) => expr_requires_semi_to_be_stmt(e),
    ast::StmtKind::Semi(..) => false,
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

// a bunch of utility functions of the form parse_<thing>_from_<source>
// where <thing> includes crate, expr, item, stmt, tts, and one that
// uses a HOF to parse anything, and <source> includes file and
// source_str.

pub fn parse_package_from_file<'a>(input: &Path, sess: &'a ParseSess) -> PResult<'a, ast::Package> {
    let mut parser = new_parser_from_file(sess, input);
    parser.parse_package()
}

pub fn parse_package_from_source_str<'a>(name: String, source: String, sess: &'a ParseSess)
                                       -> PResult<'a, ast::Package> {
    new_parser_from_source_str(sess, name, source).parse_package()
}

pub fn parse_expr_from_source_str<'a>(name: String, source: String, sess: &'a ParseSess)
                                      -> PResult<'a, P<ast::Expr>> {
    new_parser_from_source_str(sess, name, source).parse_expr()
}

/// Parses an item.
///
/// Returns `Ok(Some(item))` when successful, `Ok(None)` when no item was found, and`Err`
/// when a syntax error occurred.
pub fn parse_item_from_source_str<'a>(name: String, source: String, sess: &'a ParseSess)
                                      -> PResult<'a, Option<P<ast::Item>>> {
    new_parser_from_source_str(sess, name, source).parse_item()
}

pub fn parse_stmt_from_source_str<'a>(name: String, source: String, sess: &'a ParseSess)
                                      -> PResult<'a, Option<ast::Stmt>> {
    new_parser_from_source_str(sess, name, source).parse_full_stmt()
}

// Warning: This parses with quote_depth > 0, which is not the default.
pub fn parse_tts_from_source_str<'a>(name: String,
                                     source: String,
                                     sess: &'a ParseSess)
                                     -> PResult<'a, Vec<tokenstream::TokenTree>> {
  let mut p = new_parser_from_source_str(
    sess,
    name,
    source
  );
  p.quote_depth += 1;
  // right now this is re-creating the token trees from ... token trees.
  p.parse_all_token_trees()
}

// Create a new parser from a source string
pub fn new_parser_from_source_str<'a>(sess: &'a ParseSess,
                                      name: String,
                                      source: String)
                                      -> Parser<'a> {
  filemap_to_parser(sess, sess.codemap().new_filemap(name, None, source))
}

/// Create a new parser, handling errors as appropriate
/// if the file doesn't exist
pub fn new_parser_from_file<'a>(sess: &'a ParseSess, path: &Path) -> Parser<'a> {
    filemap_to_parser(sess, file_to_filemap(sess, path, None))
}

/// Given a filemap and config, return a parser
pub fn filemap_to_parser<'a>(sess: &'a ParseSess,
                             filemap: Rc<FileMap>) -> Parser<'a> {
  debug!("start_pos: {:?}, end_pos: {:?}", filemap.start_pos, filemap.end_pos);
  let end_pos = filemap.end_pos;
  let mut parser = tts_to_parser(sess, filemap_to_tts(sess, filemap));

  if parser.token == token::Eof && parser.span == codespan::DUMMY_SPAN {
    parser.span = mk_span(end_pos, end_pos);
  }

  parser
}

// base abstractions

/// Given a session and a path and an optional span (for error reporting),
/// add the path to the session's codemap and return the new filemap.
fn file_to_filemap(sess: &ParseSess, path: &Path, spanopt: Option<Span>)
                   -> Rc<FileMap> {
    match sess.codemap().load_file(path) {
        Ok(filemap) => filemap,
        Err(e) => {
            let msg = format!("couldn't read {:?}: {}", path.display(), e);
            match spanopt {
                Some(sp) => panic!(sess.span_diagnostic.span_fatal(sp, &msg)),
                None => panic!(sess.span_diagnostic.fatal(&msg))
            }
        }
    }
}

/// Given a filemap, produce a sequence of token-trees
pub fn filemap_to_tts(sess: &ParseSess, filemap: Rc<FileMap>)
  -> Vec<tokenstream::TokenTree> {
  // it appears to me that the cfg doesn't matter here... indeed,
  // parsing tt's probably shouldn't require a parser at all.
  let srdr = StringReader::new(&sess.span_diagnostic, filemap);
  let mut p1 = Parser::new(sess, Box::new(srdr));
  p1.parse_all_token_trees().ok().unwrap()
}

/// Given tts and cfg, produce a parser
pub fn tts_to_parser<'a>(sess: &'a ParseSess,
                         tts: Vec<tokenstream::TokenTree>) -> Parser<'a> {
  let trdr = ttreader::new_tt_reader(&sess.span_diagnostic, None, tts);
  Parser::new(sess, Box::new(trdr))
}

#[cfg(test)]
mod tests {
  use std::io::{Read, Write};
  use std::ops::FnOnce;
  use std::rc::Rc;

  use ast::{Expr, Package};
  use ast_printer::{self, NoAnn};
  use codespan::CodeMap;
  use lexer::{StringReader};
  use ptr::P;
  use token;
  use super::{ParseSess, Parser, PResult};


  /// Given tts and cfg, produce a parser
  fn str_to_parser(sess: &ParseSess, src: String) -> Parser {
    let r = StringReader::new_from_str(src, &sess.span_diagnostic);
    Parser::new(sess, Box::new(r))
  }

  fn str_to<F, T>(src: &str, f: F) -> T where F: FnOnce(&mut Parser) -> T {
    let sess: ParseSess = ParseSess::new();
    let mut parser = str_to_parser(&sess, src.to_string());
    f(&mut parser)
  }

  fn str_to_pp(src: &str) -> String {
    let sess: ParseSess = ParseSess::new();
    let mut codemap = CodeMap::new();
    let filemap = codemap.new_filemap("".to_string(), None, src.to_string());
    let reader = StringReader::new(&sess.span_diagnostic, filemap);
    let mut parser = Parser::new(&sess, Box::new(reader));

    let package = parser.parse_package().ok().unwrap();

    let rdr: Vec<u8> = src.to_string().into();
    let mut rdr = &*rdr;

    let mut out = Vec::new();
    {
      let writer: &mut Write = &mut out;
      let noAnn = NoAnn;

      ast_printer::print_package(&codemap,
                                 &sess.span_diagnostic,
                                 &package,
                                 "test".to_string(),
                                 &mut rdr,
                                 Box::new(writer),
                                 &noAnn,
                                 false);
    }

    String::from_utf8(out).expect("from_utf8 conversion error...")
  }

  fn str_to_package(src: &str) -> PResult<'a, Package> {
    str_to(src, |p| p.parse_package())
  }

  fn str_to_expr(src: &str) -> PResult<'a, P<Expr>> {
    str_to(src, |p| p.parse_expr())
  }

  #[test]
  fn test_parser_bump() {
    let src = "fn abc(aaa: Int) {}";
    let sess: ParseSess = ParseSess::new();
    let mut parser = str_to_parser(&sess, src.to_string());

    loop {
      let t = parser.token.clone();
      println!("{:?}", t);
      if t == token::Eof {
        return break;
      }
      parser.bump();
    }
  }

  #[test]
  fn test_expr_struct() {
    match str_to_expr("xyz {x: 1, y: 1, z:1}") {
      Ok(e) => println!("{:?}", e),
      Err(_) => println!("Error")
    };
  }
}
