use std::char;
use std::mem::replace;
use std::rc::Rc;

use ast::{self};
use codemap::{self, BytePos, CharPos, Span, Pos};
use token::{self, str_to_ident};

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TokenAndSpan {
  pub tok: token::Token,
  pub sp: Span,
}

pub struct FatalError;

pub trait Reader {
  fn is_eof(&self) -> bool;
  fn next_token(&mut self) -> TokenAndSpan;
  /// Report a fatal error with the current span.
  fn fatal(&self, &str) -> FatalError;
  /// Report a non-fatal error with the current span.
  fn err(&self, &str);
  fn peek(&self) -> TokenAndSpan;
  /// Get a token the parser cares about.
  fn real_token(&mut self) -> TokenAndSpan {
    let mut t = self.next_token();
    loop {
      match t.tok {
        token::Whitespace => {
          t = self.next_token();
        },
        _ => break
      }
    }
    t
  }
}

pub struct StringReader {
  /// source text
  pub source_text: Rc<String>,

  /// The absolute offset within the source text of the next character to read
  pub pos: BytePos,
  /// The absolute offset within the source text of the last character read (curr)
  pub last_pos: BytePos,
  /// The last character to be read
  pub curr: Option<char>,

  pub peek_tok: token::Token,
  pub peek_span: Span
}

impl Reader for StringReader {
  fn is_eof(&self) -> bool { self.curr.is_none() }

  fn next_token(&mut self) -> TokenAndSpan {
    let ret_val = TokenAndSpan {
      tok: replace(&mut self.peek_tok, token::Underscore),
      sp: self.peek_span,
    };
    self.advance_token();
    ret_val
  }
  /// Report a fatal error with the current span.
  fn fatal(&self, m: &str) -> FatalError {
    unimplemented!()
  }
  /// Report a non-fatal error with the current span.
  fn err(&self, m: &str) {
    unimplemented!()
  }

  fn peek(&self) -> TokenAndSpan {
    unimplemented!()
  }

  /// Get a token the parser cares about.
  fn real_token(&mut self) -> TokenAndSpan {
    let mut t = self.next_token();
    loop {
      match t.tok {
        token::Whitespace => {
          t = self.next_token();
        },
        _ => break
      }
    }
    t
  }
}

pub fn char_at(s: &str, byte: usize) -> char {
  s[byte..].chars().next().unwrap()
}


impl StringReader {
  pub fn new(source: Rc<String>) -> StringReader {
    let mut sr = StringReader {
      pos: BytePos(0),
      last_pos: BytePos(0),
      curr: Some('\n'),
      peek_tok: token::Eof,
      peek_span: codemap::DUMMY_SPAN,
      source_text: source,
    };
    sr.bump();
    sr.advance_token();
    sr
  }

  /// Report a lexical error with a given span.
  pub fn err_span(&self, sp: Span, m: &str) {
    unimplemented!()
  }

  /// Report a lexical error spanning [`from_pos`, `to_pos`).
  fn err_span_(&self, from_pos: BytePos, to_pos: BytePos, m: &str) {
    self.err_span(codemap::mk_span(from_pos, to_pos), m)
  }

  pub fn curr_is(&self, c: char) -> bool {
    self.curr == Some(c)
  }

  fn byte_offset(&self, pos: BytePos) -> BytePos {
    (pos - BytePos(0))
  }

  pub fn nextch(&self) -> Option<char> {
    let offset = self.byte_offset(self.pos).to_usize();
    if offset < self.source_text.len() {
      Some(char_at(&self.source_text, offset))
    } else {
      None
    }
  }

  pub fn nextch_is(&self, c: char) -> bool {
    self.nextch() == Some(c)
  }

  pub fn nextnextch(&self) -> Option<char> {
    let offset = self.byte_offset(self.pos).to_usize();
    let s = &self.source_text[..];

    if offset >= s.len() { return None }
    let next = offset + char_at(s, offset).len_utf8();
    if next < s.len() {
      Some(char_at(s, next))
    } else {
      None
    }
  }

  pub fn nextnextch_is(&self, c: char) -> bool {
    self.nextnextch() == Some(c)
  }

  /// Eats <XID_start><XID_continue>*, if possible.
  fn scan_optional_raw_name(&mut self) -> Option<ast::Name> {
    if !ident_start(self.curr) {
      return None;
    }
    let start = self.last_pos;
    while ident_continue(self.curr) {
      self.bump();
    }

    self.with_str_from(start, |string| {
      if string == "_" {
        None
      } else {
        Some(token::intern(string))
      }
    })
  }

  fn binop(&mut self, op: token::BinOpToken) -> token::Token {
    self.bump();
    if self.curr_is('=') {
      self.bump();
      return token::BinOpEq(op);
    } else {
      return token::BinOp(op);
    }
  }

  /// Calls `f` with a string slice of the source text spanning from `start`
  /// up to but excluding `end`.
  fn with_str_from_to<T, F>(&self, start: BytePos, end: BytePos, f: F) -> T
      where F: FnOnce(&str) -> T {
    f(&self.source_text[self.byte_offset(start).to_usize()..self.byte_offset(end).to_usize()])
  }

  /// Calls `f` with a string slice of the source text spanning from `start`
  /// up to but excluding `self.last_pos`, meaning the slice does not include
  /// the character `self.curr`.
  pub fn with_str_from<T, F>(&self, start: BytePos, f: F) -> T
      where F: FnOnce(&str) -> T {
    self.with_str_from_to(start, self.last_pos, f)
  }

  fn next_token_inner(&mut self) -> Result<token::Token, ()> {
    let c = self.curr;

    if ident_start(c) {
      let start = self.last_pos;
      self.bump();
      while ident_continue(self.curr) {
        self.bump();
      }

      return Ok(self.with_str_from(start, |string| {
        if string == "_" {
          token::Underscore
        } else {
          token::Ident(str_to_ident(string))
        }
      }));
    }

    if is_dec_digit(c) {
      let num = self.scan_number(c.unwrap());
      let suffix = self.scan_optional_raw_name();
      debug!("next_token_inner: scanned number {:?}, {:?}", num, suffix);
      return Ok(token::Literal(num, suffix));
    }

    match c.expect("next_token_inner called at EOF") {
      // One-byte tokens
      '@' => {
        self.bump();
        return Ok(token::At);
      }
      ':' => {
        self.bump();
        if self.curr_is(':') {
          self.bump();
          return Ok(token::ModSep);
        }
        return Ok(token::Colon);
      }
      ';' => {
        self.bump();
        return Ok(token::SemiColon);
      }
      ',' => {
        self.bump();
        return Ok(token::Comma);
      }
      '.' => {
        self.bump();
        if self.curr_is('.') {
          self.bump();
          if self.curr_is('.') {
            self.bump();
            return Ok(token::DotDotDot);
          } else {
            return Ok(token::DotDot);
          }
        } else {
          return Ok(token::Dot);
        }
      }
      '$' => {
        self.bump();
        return Ok(token::Dollar);
      }
      '#' => {
        self.bump();
        return Ok(token::Pound);
      }
      '?' => {
        self.bump();
        return Ok(token::Question);
      }

      '(' => {
        self.bump();
        return Ok(token::OpenDelim(token::Paren));
      }
      ')' => {
        self.bump();
        return Ok(token::CloseDelim(token::Paren));
      }
      '{' => {
        self.bump();
        return Ok(token::OpenDelim(token::Brace));
      }
      '}' => {
        self.bump();
        return Ok(token::CloseDelim(token::Brace));
      }
      '[' => {
        self.bump();
        return Ok(token::OpenDelim(token::Bracket));
      }
      ']' => {
        self.bump();
        return Ok(token::CloseDelim(token::Bracket));
      }

      // Multi-byte tokens
      '=' => {
        self.bump();
        if self.curr_is('=') {
          self.bump();
          return Ok(token::EqEq);
        } else if self.curr_is('>') {
          self.bump();
          return Ok(token::FatArrow);
        } else {
          return Ok(token::Eq);
        }
      }
      '!' => {
        self.bump();
        if self.curr_is('=') {
          self.bump();
          return Ok(token::Ne);
        } else {
          return Ok(token::Not);
        }
      }
      '<' => {
        self.bump();
        match self.curr.unwrap_or('\x00') {
          '=' => {
            self.bump();
            return Ok(token::Le);
          }
          '<' => {
            return Ok(self.binop(token::LShift));
          }
          '>' => {
            self.bump();
            return Ok(token::Ne);
          }
          '-' => {
            self.bump();
            return Ok(token::LArrow);
          }
          _ => {
            return Ok(token::Lt);
          }
        }
      }
      '>' => {
        self.bump();
        match self.curr.unwrap_or('\x00') {
          '=' => {
            self.bump();
            return Ok(token::Ge);
          }
          '>' => {
            return Ok(self.binop(token::RShift));
          }
          _ => {
            return Ok(token::Gt);
          }
        }
      }
      '-' => {
        if self.nextch_is('>') {
          self.bump();
          self.bump();
          return Ok(token::RArrow);
        } else {
          return Ok(self.binop(token::Minus));
        }
      }
      c => {

      }
    }

    unimplemented!()
  }

  fn advance_token(&mut self) -> Result<(), ()> {
    match self.scan_whitespace_or_comment() {
      Some(ws_or_comment) => {
        self.peek_span = ws_or_comment.sp;
        self.peek_tok = ws_or_comment.tok;
      }
      None => {
        if self.is_eof() {
          self.peek_tok = token::Eof;
          self.peek_span = codemap::mk_span(self.last_pos, self.last_pos);
        } else {
          let start_bytespos = self.last_pos;
          self.peek_tok = self.next_token_inner()?;
          self.peek_span = codemap::mk_span(start_bytespos, self.last_pos)
        }
      }
    }
    Ok(())
  }

  fn bump(&mut self) {
    self.last_pos = self.pos;
    let current_byte_offset = self.byte_offset(self.pos).to_usize();
    if current_byte_offset < self.source_text.len() {
      let ch = char_at(&self.source_text, current_byte_offset);
      let next = current_byte_offset + ch.len_utf8();
      let byte_offset_diff = next - current_byte_offset;
      self.pos = self.pos + Pos::from_usize(byte_offset_diff);
      self.curr = Some(ch);
    } else {
      self.curr = None;
    }
  }

  /// Create a Name from a given offset to the current offset, each
  /// adjusted 1 towards each other (assumes that on either side there is a
  /// single-byte delimiter).
  pub fn name_from(&self, start: BytePos) -> ast::Name {
    debug!("taking an ident from {:?} to {:?}", start, self.last_pos);
    self.with_str_from(start, token::intern)
  }

  /// If there is whitespace or a comment, scan it. Otherwise, return None.
  pub fn scan_whitespace_or_comment(&mut self) -> Option<TokenAndSpan> {
    match self.curr.unwrap_or('\0') {
      '/' | '#' => {
        unimplemented!()
      }
      c if is_whitespace(Some(c)) => {
        let start_bpos = self.last_pos;
        while is_whitespace(self.curr) { self.bump(); }

        Some(TokenAndSpan {
          tok: token::Whitespace,
          sp: codemap::mk_span(start_bpos, self.last_pos)
        })
      }
      _ => None
    }
  }

  /// Lex a LIT_INTEGER or a LIT_FLOAT
  fn scan_number(&mut self, c: char) -> token::Lit {
    let num_digits;
    let mut base = 10;
    let start_bpos = self.last_pos;

    self.bump();

    if c == '0' { // if starts with '0'
      match self.curr.unwrap_or('\0') {
        'b' => {
          self.bump();
          base = 2;
          num_digits = self.scan_digits(2, 10);
        }
        'o' => {
          self.bump();
          base = 8;
          num_digits = self.scan_digits(8, 10);
        }
        'x' => {
          self.bump();
          base = 16;
          num_digits = self.scan_digits(16, 16);
        }
        '0'...'9' | '_' | '.' => {
          num_digits = self.scan_digits(10, 10) + 1;
        }
        _ => {
          // just a 0
          return token::Integer(self.name_from(start_bpos));
        }
      }
    } else if c.is_digit(10) {
      num_digits = self.scan_digits(10, 10) + 1;
    } else {
      num_digits = 0;
    }

    if num_digits == 0 {
      self.err_span_(start_bpos,
                     self.last_pos,
                     "no valid digits found for number");
      return token::Integer(token::intern("0"));
    }

    // might be a float, but don't be greedy if this is actually an
    // integer literal followed by field/method access or a range pattern
    // (`0..2` and `12.foo()`)
    if self.curr_is('.') && !self.nextch_is('.') &&
      !self.nextch()
           .unwrap_or('\0')
           .is_xid_start() {
      // might have stuff after the ., and if it does, it needs to start
      // with a number
      self.bump();
      if self.curr.unwrap_or('\0').is_digit(10) {
        self.scan_digits(10, 10);
        self.scan_float_exponent();
      }
      let last_pos = self.last_pos;
      self.check_float_base(start_bpos, last_pos, base);
      return token::Float(self.name_from(start_bpos));
    } else {
      // it might be a float if it has an exponent
      if self.curr_is('e') || self.curr_is('E') {
        self.scan_float_exponent();
        let last_pos = self.last_pos;
        self.check_float_base(start_bpos, last_pos, base);
        return token::Float(self.name_from(start_bpos));
      }
      // but we certainly have an integer!
      return token::Integer(self.name_from(start_bpos));
    }
  }

  /// Scan over a float exponent.
  fn scan_float_exponent(&mut self) {
    if self.curr_is('e') || self.curr_is('E') {
      self.bump();
      if self.curr_is('-') || self.curr_is('+') {
        self.bump();
      }
      if self.scan_digits(10, 10) == 0 {
        self.err_span_(self.last_pos,
                       self.pos,
                       "expected at least one digit in exponent")
      }
    }
  }

  /// Check that a base is valid for a floating literal, emitting a nice
  /// error if it isn't.
  fn check_float_base(&mut self, start_bpos: BytePos, last_bpos: BytePos, base: usize) {
    match base {
      16 => {
        self.err_span_(start_bpos,
                       last_bpos,
                       "hexadecimal float literal is not supported")
      }
      8 => {
        self.err_span_(start_bpos,
                       last_bpos,
                       "octal float literal is not supported")
      }
      2 => {
        self.err_span_(start_bpos,
                       last_bpos,
                       "binary float literal is not supported")
      }
      _ => (),
    }
  }

  /// Scan through any digits (base `scan_radix`) or underscores,
  /// and return how many digits there were.
  ///
  /// `real_radix` represents the true radix of the number we're
  /// interested in, and errors will be emitted for any digits
  /// between `real_radix` and `scan_radix`.
  fn scan_digits(&mut self, real_radix: u32, scan_radix: u32) -> usize {
    assert!(real_radix <= scan_radix);
    let mut len = 0;
    loop {
      let c = self.curr;

      if c == Some('_') { // e.g.) 100_000_000
        self.bump();
        continue;
      }
      match c.and_then(|cc| cc.to_digit(scan_radix)) {
        Some(_) => {
          if c.unwrap().to_digit(real_radix).is_none() {
            self.err_span_(self.last_pos,
                           self.pos,
                           &format!("invalid digit for a base {} literal", real_radix));
          }
          len += 1;
          self.bump();

        }
        _ => return len,
      }
    }
  }
}

fn in_range(c: Option<char>, lo: char, hi: char) -> bool {
  match c {
    Some(c) => lo <= c && c <= hi,
    _ => false,
  }
}

fn is_dec_digit(c: Option<char>) -> bool {
  return in_range(c, '0', '9');
}

// The first character of identifiers should start with one of [a-zA-Z_\x??].
fn ident_start(c: Option<char>) -> bool {
  let c = match c { Some(c) => c, None => return false };

  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || c == '_'
  || (c > '\x7f' && c.is_xid_start()) // unicode
}

// The subsequent character of identifiers can be one of [a-zA-Z0-9_\x??].
fn ident_continue(c: Option<char>) -> bool {
  let c = match c { Some(c) => c, None => return false };

  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')
  || c == '_'
  || (c > '\x7f' && c.is_xid_continue())
}

pub fn is_whitespace(c: Option<char>) -> bool {
  match c.unwrap_or('\x00') { // None can be null for now... it's not whitespace
    ' ' | '\n' | '\t' | '\r' => true,
    _ => false
  }
}

#[cfg(test)]
mod tests {
  use ast;
  use token::{self, str_to_ident};
  use std::rc::Rc;
  use super::{Reader, StringReader};

  fn assert_tokens(source: &str, expected: &[token::Token]) {
    let mut result: Vec<token::Token> = Vec::new();

    let mut sr = StringReader::new(Rc::new(source.to_string()));

    loop {
      let tok = sr.next_token().tok;
      if tok == token::Eof { break;}
      result.push(tok.clone());
    }

    assert_eq!(&result[..], expected)
  }

  fn ident(name: &str) -> token::Token {
    token::Ident(str_to_ident(name))
  }

  #[test]
  fn test_tokens() {
    assert_tokens("@:;,.$?",
                  &[token::At, token::Colon, token::SemiColon, token::Comma, token::Dot,
                  token::Dollar, token::Question]);
    // start with .
    assert_tokens("....", &[token::DotDotDot, token::Dot]);

    assert_tokens("{([])}", &[
      token::OpenDelim(token::Brace),
      token::OpenDelim(token::Paren),
      token::OpenDelim(token::Bracket),
      token::CloseDelim(token::Bracket),
      token::CloseDelim(token::Paren),
      token::CloseDelim(token::Brace)
    ]);

    // start with =
    assert_tokens("=>===",
                  &[token::FatArrow, token::EqEq, token::Eq]);
    // start with !
    assert_tokens("!=!",
                  &[token::Ne, token::Not]);
    // start with <
    assert_tokens("<=<<<><-<",
                  &[token::Le, token::BinOp(token::LShift), token::Ne, token::LArrow, token::Lt]);
    // start with >
    assert_tokens("=>>>>",
                  &[token::FatArrow, token::BinOp(token::RShift), token::Gt]);
    // start with -
    assert_tokens("->-",
                  &[token::RArrow, token::BinOp(token::Minus)]);
  }

  #[test]
  fn test_idents() {
    assert_tokens("let", &[ident("let")]);
  }

  fn assert_lit_integer(expected_lit: &str, expected_suffix: Option<&str>, src: &str) {
    assert_tokens(src,
      &[token::Literal(token::Integer(intern(expected_lit)),
                       expected_suffix.and_then(|s| Some(intern(s))))]);
  }

  fn intern(str: &str) -> ast::Name {
    token::intern(str)
  }

  #[test]
  fn test_lit() {
    assert_lit_integer("1", None, "1");
    assert_lit_integer("98_222", None, "98_222");
    assert_lit_integer("0xff", None, "0xff");
    assert_lit_integer("0b1111_0000",  None, "0b1111_0000");

    // type suffix
    assert_lit_integer("1", Some("u8"), "1u8");
    assert_lit_integer("98_222",  Some("u32"), "98_222u32");
    assert_lit_integer("0xff",  Some("u64"), "0xffu64");
  }
}

