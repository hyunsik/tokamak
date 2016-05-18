use std::char;
use std::rc::Rc;

use codemap::{BytePos, Span};
use token;

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
}

impl Reader for StringReader {
  fn is_eof(&self) -> bool { self.curr.is_none() }

  fn next_token(&mut self) -> TokenAndSpan {
    TokenAndSpan {
      tok: self.next_token_inner(),
      sp: Span {lo: BytePos(0), hi: BytePos(0)}
    }
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

fn char_at(s: &str, byte: usize) -> char {
  s[byte..].chars().next().unwrap()
}


impl StringReader {
  pub fn new(source: Rc<String>) -> StringReader {
    let mut sr = StringReader {
      source_text: source,
      pos: BytePos(0),
      last_pos: BytePos(0),
      curr: None
    };
    sr.bump();
    sr
  }

  fn byte_offset(&self, pos: BytePos) -> BytePos {
    (pos - BytePos(0))
  }

  pub fn curr_is(&self, c: char) -> bool {
    self.curr == Some(c)
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

  fn next_token_inner(&mut self) -> token::Token {
    let c = self.curr;

    if ident_start(c) {
      let start = self.last_pos;
      self.bump();
      while ident_continue(self.curr) {
        self.bump();
      }

      return self.with_str_from(start, |string| {
        token::Ident
      });
    }

    match c.expect("next_token_inner called at EOF") {
      // One-byte tokens
      '@' => {
        self.bump();
        return token::At;
      }
      ':' => {
        self.bump();
        if self.curr_is(':') {
          self.bump();
          return token::ModSep;
        }
        return token::Colon;
      }
      ';' => {
        self.bump();
        return token::SemiColon;
      }
      ',' => {
        self.bump();
        return token::Comma;
      }
      '.' => {
        self.bump();
        if self.curr_is('.') {
          self.bump();
          if self.curr_is('.') {
            self.bump();
            return token::DotDotDot;
          } else {
            return token::DotDot;
          }
        } else {
          return token::Dot;
        }
      }
      '$' => {
        self.bump();
        return token::Dollar;
      }
      '#' => {
        self.bump();
        return token::Pound;
      }
      '?' => {
        self.bump();
        return token::Question;
      }

      '(' => {
        self.bump();
        return token::OpenDelim(token::Paren);
      }
      ')' => {
        self.bump();
        return token::CloseDelim(token::Paren);
      }
      '{' => {
        self.bump();
        return token::OpenDelim(token::Brace);
      }
      '}' => {
        self.bump();
        return token::CloseDelim(token::Brace);
      }
      '[' => {
        self.bump();
        return token::OpenDelim(token::Bracket);
      }
      ']' => {
        self.bump();
        return token::CloseDelim(token::Bracket);
      }

      // Multi-byte tokens
      '=' => {
        self.bump();
        if self.curr_is('=') {
          self.bump();
          return token::EqEq;
        } else if self.curr_is('>') {
          self.bump();
          return token::FatArrow;
        } else {
          return token::Eq;
        }
      }
      '!' => {
        self.bump();
        if self.curr_is('=') {
          self.bump();
          return token::Ne;
        } else {
          return token::Not;
        }
      }
      '<' => {
        self.bump();
        match self.curr.unwrap_or('\x00') {
          '=' => {
            self.bump();
            return token::Le;
          }
          '<' => {
            return self.binop(token::LShift);
          }
          '>' => {
            self.bump();
            return token::Ne;
          }
          '-' => {
            self.bump();
            return token::LArrow;
          }
          _ => {
            return token::Lt;
          }
        }
      }
      '>' => {
        self.bump();
        match self.curr.unwrap_or('\x00') {
          '=' => {
            self.bump();
            return token::Ge;
          }
          '>' => {
            return self.binop(token::RShift);
          }
          _ => {
            return token::Gt;
          }
        }
      }
      '-' => {
        if self.nextch_is('>') {
          self.bump();
          self.bump();
          return token::RArrow;
        } else {
          return self.binop(token::Minus);
        }
      }
      c => {

      }
    }

    unimplemented!()
  }

  fn bump(&mut self) {
    let current_byte_offset = self.pos.to_usize();

    if current_byte_offset < self.source_text.len() {
      let ch = char_at(&self.source_text, current_byte_offset);
      self.pos = self.pos + BytePos(1);
      self.curr = Some(ch);
    } else {
      self.curr = None;
    }
  }
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

#[cfg(test)]
mod tests {
  use token;
  use std::rc::Rc;
  use super::{Reader, StringReader};

  fn assert_tokens(source: &str, expected: &[token::Token]) {
    let mut result: Vec<token::Token> = Vec::new();

    let mut sr = StringReader::new(Rc::new(source.to_string()));
    while !sr.is_eof() {
      result.push(sr.next_token().tok);
    }

    assert_eq!(&result[..], expected)
  }

  #[test]
  fn test_tokens() {
    assert_tokens("@:;,.$#?",
                  &[token::At, token::Colon, token::SemiColon, token::Comma, token::Dot,
                  token::Dollar, token::Pound, token::Question]);
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
    assert_tokens("let;", &[token::Ident]);
  }

  #[test]
  fn test_bump() {
    let src = "var x = 1;";
    let src_str = src.to_string();
    let mut sr = StringReader::new(Rc::new("var x = 1;".to_string()));
    let mut pos = 0;
    while !sr.is_eof() {
      sr.bump();
      pos += 1;
    }

    assert_eq!(pos, src.len());
  }
}


