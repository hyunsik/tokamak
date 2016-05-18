pub use self::BinOpToken::*;
pub use self::DelimToken::*;
pub use self::Token::*;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum BinOpToken {
  Plus,
  Minus,
  Star,
  Slash,
  LShift,
  RShift
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum DelimToken {
  /// A round parenthesis: `(` or `)`
  Paren,
  /// A square bracket: `[` or `]`
  Bracket,
  /// A curly brace: `{` or `}`
  Brace,
}

#[derive(Clone, PartialEq, Eq, Debug)]
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

  /* Name components */
  Ident,
  Underscore,

  /// Whitespace
  Whitespace,
  /// Comment
  Comment,

  /// End of file
  Eof,
}