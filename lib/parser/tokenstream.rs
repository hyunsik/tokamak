use std::rc::Rc;

use ast::{self, AttrStyle, LitKind};
use codemap::Spanned;
use common::codespan::Span;
use comments::{doc_comment_style, strip_doc_comment_decoration};
use token::{self, Lit, Token};
use parser;


/// A delimited sequence of token trees
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Delimited {
  /// The type of delimiter
  pub delim: token::DelimToken,
  /// The span covering the opening delimiter
  pub open_span: Span,
  /// The delimited sequence of token trees
  pub tts: Vec<TokenTree>,
  /// The span covering the closing delimiter
  pub close_span: Span,
}

impl Delimited {
  /// Returns the opening delimiter as a token.
  pub fn open_token(&self) -> token::Token {
    token::OpenDelim(self.delim)
  }

  /// Returns the closing delimiter as a token.
  pub fn close_token(&self) -> token::Token {
    token::CloseDelim(self.delim)
  }

  /// Returns the opening delimiter as a token tree.
  pub fn open_tt(&self) -> TokenTree {
    TokenTree::Token(self.open_span, self.open_token())
  }

  /// Returns the closing delimiter as a token tree.
  pub fn close_tt(&self) -> TokenTree {
    TokenTree::Token(self.close_span, self.close_token())
  }

  /// Returns the token trees inside the delimiters.
  pub fn subtrees(&self) -> &[TokenTree] {
    &self.tts
  }
}

/// A sequence of token trees
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct SequenceRepetition {
  /// The sequence of token trees
  pub tts: Vec<TokenTree>,
  /// The optional separator
  pub separator: Option<token::Token>,
  /// Whether the sequence can be repeated zero (*), or one or more times (+)
  pub op: KleeneOp,
  /// The number of `MatchNt`s that appear in the sequence (and subsequences)
  pub num_captures: usize,
}

/// A Kleene-style [repetition operator](http://en.wikipedia.org/wiki/Kleene_star)
/// for token sequences.
#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub enum KleeneOp {
  ZeroOrMore,
  OneOrMore,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenTree {
  /// A single token
  Token(Span, token::Token),
  /// A delimited sequence of token trees
  Delimited(Span, Rc<Delimited>),

  // This only makes sense in MBE macros.
  /// A kleene-style repetition sequence with a span
  Sequence(Span, Rc<SequenceRepetition>),
}

impl TokenTree {
  pub fn len(&self) -> usize {
    match *self {
      TokenTree::Token(_, token::DocComment(name)) => {
        match doc_comment_style(&name.as_str()) {
          AttrStyle::Outer => 2,
          AttrStyle::Inner => 3,
        }
      }
      TokenTree::Delimited(_, ref delimed) => delimed.tts.len() + 2,
      TokenTree::Sequence(_, ref seq) => seq.tts.len(),
      TokenTree::Token(..) => 0,
    }
  }

  pub fn get_tt(&self, index: usize) -> TokenTree {
    match (self, index) {
      (&TokenTree::Token(sp, token::DocComment(_)), 0) => TokenTree::Token(sp, token::Pound),
      (&TokenTree::Token(sp, token::DocComment(name)), 1)
      if doc_comment_style(&name.as_str()) == AttrStyle::Inner => {
        TokenTree::Token(sp, token::Not)
      }
      (&TokenTree::Token(sp, token::DocComment(name)), _) => {
        let stripped = strip_doc_comment_decoration(&name.as_str());

        // Searches for the occurrences of `"#*` and returns the minimum number of `#`s
        // required to wrap the text.
        let num_of_hashes = stripped.chars()
          .scan(0, |cnt, x| {
            *cnt = if x == '"' {
              1
            } else if *cnt != 0 && x == '#' {
              *cnt + 1
            } else {
              0
            };
            Some(*cnt)
          })
          .max()
          .unwrap_or(0);

        TokenTree::Delimited(sp, Rc::new(Delimited {
          delim: token::Bracket,
          open_span: sp,
          tts: vec![TokenTree::Token(sp, token::Ident(token::str_to_ident("doc"))),
                              TokenTree::Token(sp, token::Eq),
                              TokenTree::Token(sp, token::Literal(
                                  token::StrRaw(token::intern(&stripped), num_of_hashes), None))],
          close_span: sp,
        }))
      }
      (&TokenTree::Delimited(_, ref delimed), _) => {
        if index == 0 {
          return delimed.open_tt();
        }
        if index == delimed.tts.len() + 1 {
          return delimed.close_tt();
        }
        delimed.tts[index - 1].clone()
      }
      (&TokenTree::Sequence(_, ref seq), _) => seq.tts[index].clone(),
      _ => unimplemented!()
    }
  }

  /// Returns the `Span` corresponding to this token tree.
  pub fn get_span(&self) -> Span {
    match *self {
      TokenTree::Token(span, _) => span,
      TokenTree::Delimited(span, _) => span,
      TokenTree::Sequence(span, _) => span,
    }
  }

  /// Check if this TokenTree is equal to the other, regardless of span information.
  pub fn eq_unspanned(&self, other: &TokenTree) -> bool {
    match (self, other) {
      (&TokenTree::Token(_, ref tk), &TokenTree::Token(_, ref tk2)) => tk == tk2,
      (&TokenTree::Delimited(_, ref dl), &TokenTree::Delimited(_, ref dl2)) => {
        (*dl).delim == (*dl2).delim && dl.tts.len() == dl2.tts.len() &&
          {
            for (tt1, tt2) in dl.tts.iter().zip(dl2.tts.iter()) {
              if !tt1.eq_unspanned(tt2) {
                return false;
              }
            }
            true
          }
      }
      (_, _) => false,
    }
  }

  /// Retrieve the TokenTree's span.
  pub fn span(&self) -> Span {
    match *self {
      TokenTree::Token(sp, _) |
      TokenTree::Delimited(sp, _) |
      TokenTree::Sequence(sp, _) => sp,
    }
  }

  /// Indicates if the stream is a token that is equal to the provided token.
  pub fn eq_token(&self, t: Token) -> bool {
    match *self {
      TokenTree::Token(_, ref tk) => *tk == t,
      _ => false,
    }
  }

  /// Indicates if the token is an identifier.
  pub fn is_ident(&self) -> bool {
    self.maybe_ident().is_some()
  }

  /// Returns an identifier.
  pub fn maybe_ident(&self) -> Option<ast::Ident> {
    match *self {
      TokenTree::Token(_, Token::Ident(t)) => Some(t.clone()),
      TokenTree::Delimited(_, ref dl) => {
        let tts = dl.subtrees();
        if tts.len() != 1 {
          return None;
        }
        tts[0].maybe_ident()
      }
      _ => None,
    }
  }

  /// Returns a Token literal.
  pub fn maybe_lit(&self) -> Option<token::Lit> {
    match *self {
      TokenTree::Token(_, Token::Literal(l, _)) => Some(l.clone()),
      TokenTree::Delimited(_, ref dl) => {
        let tts = dl.subtrees();
        if tts.len() != 1 {
          return None;
        }
        tts[0].maybe_lit()
      }
      _ => None,
    }
  }

  /// Returns an AST string literal.
  pub fn maybe_str(&self) -> Option<ast::Lit> {
    match *self {
      TokenTree::Token(sp, Token::Literal(Lit::Str_(s), _)) => {
        let l = LitKind::Str(token::intern_and_get_ident(&parser::str_lit(&s.as_str())),
                             ast::StrStyle::Cooked);
        Some(Spanned {
          node: l,
          span: sp,
        })
      }
      TokenTree::Token(sp, Token::Literal(Lit::StrRaw(s, n), _)) => {
        let l = LitKind::Str(token::intern_and_get_ident(&parser::raw_str_lit(&s.as_str())),
                             ast::StrStyle::Raw(n));
        Some(Spanned {
          node: l,
          span: sp,
        })
      }
      _ => None,
    }
  }
}