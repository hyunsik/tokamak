pub use self::BinOpToken::*;
pub use self::DelimToken::*;
pub use self::Lit::*;
pub use self::Token::*;

use rustc_serialize::{Decodable, Decoder, Encodable, Encoder};

use std::fmt;
use std::iter;
use std::ops::Deref;
use std::rc::Rc;

use ast;
use symbol::keywords;

#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub enum BinOpToken {
  Plus,
  Minus,
  Star,
  Slash,
  Percent,
  Caret,
  And,
  Or,
  LShift,
  RShift
}

/// A delimiter token
#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub enum DelimToken {
  /// A round parenthesis: `(` or `)`
  Paren,
  /// A square bracket: `[` or `]`
  Bracket,
  /// A curly brace: `{` or `}`
  Brace,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub enum Lit {
  Byte(ast::Name),
  Char(ast::Name),
  Integer(ast::Name),
  Float(ast::Name),
  Str_(ast::Name),
  StrRaw(ast::Name, usize), /* raw str delimited by n hash symbols */
  ByteStr(ast::Name),
  ByteStrRaw(ast::Name, usize), /* raw byte str delimited by n hash symbols */
}

impl Lit {
  pub fn short_name(&self) -> &'static str {
    match *self {
      Byte(_) => "byte",
      Char(_) => "char",
      Integer(_) => "integer",
      Float(_) => "float",
      Str_(_) | StrRaw(..) => "string",
      ByteStr(_) | ByteStrRaw(..) => "byte string"
    }
  }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Token {
  /* Expression-operator symbols. */
  Eq,
  EqEq,
  Ne,
  Lt,
  Le,
  Ge,
  Gt,
  AndAnd,
  OrOr,
  Not,
  Tilde,
  BinOp(BinOpToken),
  BinOpEq(BinOpToken), // e.g. '+='

  /* Structural symbols */
  At,       // @
  Colon,    // :
  SemiColon,// ;
  Comma,    // ,
  Dot,      // .
  DotDot,   // ..
  DotDotDot,// ...
  Dollar,   // $
  Pound,    // #
  Question, // ?
  ModSep,   // ::
  LArrow,   // <-
  RArrow,   // ->
  FatArrow, // =>

  /// An opening delimiter, eg. `{`
  OpenDelim(DelimToken),
  /// A closing delimiter, eg. `}`
  CloseDelim(DelimToken),

  /* Literals */
  Literal(Lit, Option<ast::Name>),

  /* Name components */
  Ident(ast::Ident),
  Underscore,

  /// Whitespace
  Whitespace,
  // Can be expanded into several tokens.
  /// Doc comment
  DocComment(ast::Name),
  /// Comment
  Comment,

  /// End of file
  Eof,
}

impl Token {

  /// Returns `true` if the token can appear at the start of an expression.
  pub fn can_begin_expr(&self) -> bool {
    match *self {
      OpenDelim(_)                => true,
      Ident(..)                   => true,
      Underscore                  => true,
      Tilde                       => true,
      Literal(_, _)               => true,
      Not                         => true,
      BinOp(Minus)                => true,
      BinOp(Star)                 => true,
      BinOp(And)                  => true,
      BinOp(Or)                   => true, // in lambda syntax
      OrOr                        => true, // in lambda syntax
      AndAnd                      => true, // double borrow
      DotDot | DotDotDot          => true, // range notation
      ModSep                      => true,
      Pound                       => true, // for expression attributes
      _                           => false,
    }
  }

  /// Returns `true` if the token is any literal
  pub fn is_lit(&self) -> bool {
    match *self {
      Literal(_, _) => true,
      _          => false,
    }
  }

  /// Returns `true` if the token is an identifier.
  pub fn is_ident(&self) -> bool {
    match *self {
      Ident(_)    => true,
      _           => false,
    }
  }

  /// Returns `true` if the token is an interpolated path.
  pub fn is_path(&self) -> bool {
    match *self {
      //Interpolated(NtPath(..))    => true,
      _                           => false,
    }
  }

  pub fn is_path_start(&self) -> bool {
    self == &ModSep || self == &Lt ||
    self.is_path_segment_keyword() || self.is_ident() && !self.is_any_keyword()
  }

  pub fn is_path_segment_keyword(&self) -> bool {
    match *self {
      Ident(id) => id.name == keywords::Super.name() ||
      id.name == keywords::SelfValue.name() ||
      id.name == keywords::SelfType.name(),
      _ => false,
    }
  }

  /// Returns `true` if the token is a given keyword, `kw`.
  pub fn is_keyword(&self, kw: keywords::Keyword) -> bool {
    match *self {
      Ident(id) => id.name == kw.name(),
      _ => false,
    }
  }

  /// Returns `true` if the token is either a strict or reserved keyword.
  pub fn is_any_keyword(&self) -> bool {
    self.is_strict_keyword() || self.is_reserved_keyword()
  }

  /// Returns `true` if the token is a strict keyword.
  pub fn is_strict_keyword(&self) -> bool {
    match *self {
      Ident(id) => id.name >= keywords::As.name() &&
      id.name <= keywords::While.name(),
      _ => false,
    }
  }

  /// Returns `true` if the token is a keyword reserved for possible future use.
  pub fn is_reserved_keyword(&self) -> bool {
    match *self {
      Ident(id) => id.name >= keywords::Abstract.name() &&
      id.name <= keywords::Yield.name(),
      _ => false,
    }
  }
}

pub fn binop_to_string(op: BinOpToken) -> &'static str {
  match op {
    Plus     => "+",
    Minus    => "-",
    Star     => "*",
    Slash    => "/",
    Percent  => "%",
    Caret    => "^",
    And      => "&",
    Or       => "|",
    LShift   => "<<",
    RShift   => ">>",
  }
}

pub fn token_to_string(tok: &Token) -> String {
  match *tok {
    Eq                   => "=".to_string(),
    Lt                   => "<".to_string(),
    Le                   => "<=".to_string(),
    EqEq                 => "==".to_string(),
    Ne                   => "!=".to_string(),
    Ge                   => ">=".to_string(),
    Gt                   => ">".to_string(),
    Not                  => "!".to_string(),
    Tilde                => "~".to_string(),
    OrOr                 => "||".to_string(),
    AndAnd               => "&&".to_string(),
    BinOp(op)            => binop_to_string(op).to_string(),
    BinOpEq(op)          => format!("{}=", binop_to_string(op)),

    /* Structural symbols */
    At                   => "@".to_string(),
    Dot                  => ".".to_string(),
    DotDot               => "..".to_string(),
    DotDotDot            => "...".to_string(),
    Comma                => ",".to_string(),
    SemiColon            => ";".to_string(),
    Colon                => ":".to_string(),
    ModSep               => "::".to_string(),
    RArrow               => "->".to_string(),
    LArrow               => "<-".to_string(),
    FatArrow             => "=>".to_string(),
    OpenDelim(Paren)     => "(".to_string(),
    CloseDelim(Paren)    => ")".to_string(),
    OpenDelim(Bracket)   => "[".to_string(),
    CloseDelim(Bracket)  => "]".to_string(),
    OpenDelim(Brace)     => "{".to_string(),
    CloseDelim(Brace)    => "}".to_string(),
    Pound                => "#".to_string(),
    Dollar               => "$".to_string(),
    Question             => "?".to_string(),

    /* Literals */
    Literal(lit, suf) => {
      let mut out = match lit {
        Byte(b)           => format!("b'{}'", b),
        Char(c)           => format!("'{}'", c),
        Float(c)          => c.to_string(),
        Integer(c)        => c.to_string(),
        Str_(s)           => format!("\"{}\"", s),
        StrRaw(s, n)      => format!("r{delim}\"{string}\"{delim}",  delim=repeat("#", n), string=s),
        ByteStr(v)        => format!("b\"{}\"", v),
        ByteStrRaw(s, n)  => format!("br{delim}\"{string}\"{delim}", delim=repeat("#", n), string=s),
      };

      if let Some(s) = suf {
        out.push_str(&s.as_str())
      }

      out
    }

    /* Name components */
    Ident(s)             => s.to_string(),
    Underscore           => "_".to_string(),

    /* Other */
    DocComment(s)        => s.to_string(),
    Comment              => "/* */".to_string(),
    Whitespace           => " ".to_string(),
    Eof                  => "<eof>".to_string(),
  }
}

fn repeat(s: &str, n: usize) -> String { iter::repeat(s).take(n).collect() }