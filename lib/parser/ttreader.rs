use std::ops::Add;
use std::rc::Rc;

use ast::{Ident};
use common::codespan::{Span, DUMMY_SPAN};
use errors::{DiagnosticBuilder, Handler};
use lexer::TokenAndSpan;
use token::{self, DocComment, Token};
use tokenstream::{self, TokenTree};

use self::LockstepIterSize::*;

///an unzipping of `TokenTree`s
#[derive(Clone)]
struct TtFrame {
  forest: TokenTree,
  idx: usize,
  dotdotdoted: bool,
  sep: Option<Token>,
}

#[derive(Clone)]
pub struct TtReader<'a> {
  pub sp_diag: &'a Handler,
  /// the unzipped tree:
  stack: Vec<TtFrame>,
  imported_from: Option<Ident>,

  // Some => return imported_from as the next token
  crate_name_next: Option<Span>,
  repeat_idx: Vec<usize>,
  repeat_len: Vec<usize>,
  /* cached: */
  pub cur_tok: Token,
  pub cur_span: Span,
  /// Transform doc comments. Only useful in macro invocations
  pub desugar_doc_comments: bool,
  pub fatal_errs: Vec<DiagnosticBuilder<'a>>,
}

#[derive(Clone)]
enum LockstepIterSize {
  LisUnconstrained,
  LisConstraint(usize, Ident),
  LisContradiction(String),
}

impl Add for LockstepIterSize {
  type Output = LockstepIterSize;

  fn add(self, other: LockstepIterSize) -> LockstepIterSize {
    match self {
      LisUnconstrained => other,
      LisContradiction(_) => self,
      LisConstraint(l_len, ref l_id) => match other {
        LisUnconstrained => self.clone(),
        LisContradiction(_) => other,
        LisConstraint(r_len, _) if l_len == r_len => self.clone(),
        LisConstraint(r_len, r_id) => {
          LisContradiction(format!("inconsistent lockstep iteration: \
                                              '{}' has {} items, but '{}' has {}",
                                   l_id, l_len, r_id, r_len))
        }
      },
    }
  }
}

fn lockstep_iter_size(t: &TokenTree, r: &TtReader) -> LockstepIterSize {
  match *t {
    TokenTree::Delimited(_, ref delimed) => {
      delimed.tts.iter().fold(LisUnconstrained, |size, tt| {
        size + lockstep_iter_size(tt, r)
      })
    },
    TokenTree::Sequence(_, ref seq) => {
      seq.tts.iter().fold(LisUnconstrained, |size, tt| {
        size + lockstep_iter_size(tt, r)
      })
    },
    TokenTree::Token(..) => LisUnconstrained,
  }
}

/// This can do Macro-By-Example transcription. On the other hand, if
/// `src` contains no `TokenTree::Sequence`s, `MatchNt`s or `SubstNt`s, `interp` can
/// (and should) be None.
pub fn new_tt_reader(sp_diag: &Handler,
                     imported_from: Option<Ident>,
                     src: Vec<tokenstream::TokenTree>)
                     -> TtReader {
  new_tt_reader_with_doc_flag(sp_diag, imported_from, src, false)
}

/// The extra `desugar_doc_comments` flag enables reading doc comments
/// like any other attribute which consists of `meta` and surrounding #[ ] tokens.
///
/// This can do Macro-By-Example transcription. On the other hand, if
/// `src` contains no `TokenTree::Sequence`s, `MatchNt`s or `SubstNt`s, `interp` can
/// (and should) be None.
pub fn new_tt_reader_with_doc_flag(sp_diag: &Handler,
                                   imported_from: Option<Ident>,
                                   src: Vec<tokenstream::TokenTree>,
                                   desugar_doc_comments: bool)
                                   -> TtReader {
  let mut r = TtReader {
    sp_diag: sp_diag,
    stack: vec!(TtFrame {
            forest: TokenTree::Sequence(DUMMY_SPAN, Rc::new(tokenstream::SequenceRepetition {
                tts: src,
                // doesn't matter. This merely holds the root unzipping.
                separator: None, op: tokenstream::KleeneOp::ZeroOrMore, num_captures: 0
            })),
            idx: 0,
            dotdotdoted: false,
            sep: None,
        }),
    imported_from: imported_from,
    crate_name_next: None,
    repeat_idx: Vec::new(),
    repeat_len: Vec::new(),
    desugar_doc_comments: desugar_doc_comments,
    /* dummy values, never read: */
    cur_tok: token::Eof,
    cur_span: DUMMY_SPAN,
    fatal_errs: Vec::new(),
  };
  tt_next_token(&mut r); /* get cur_tok and cur_span set up */
  r
}

/// Return the next token from the TtReader.
/// EFFECT: advances the reader's token field
pub fn tt_next_token(r: &mut TtReader) -> TokenAndSpan {
  // FIXME(pcwalton): Bad copy?
  let ret_val = TokenAndSpan {
    tok: r.cur_tok.clone(),
    sp: r.cur_span.clone(),
  };
  loop {
    match r.crate_name_next.take() {
      None => (),
      Some(sp) => {
        r.cur_span = sp;
        r.cur_tok = token::Ident(r.imported_from.unwrap());
        return ret_val;
      },
    }
    let should_pop = match r.stack.last() {
      None => {
        assert_eq!(ret_val.tok, token::Eof);
        return ret_val;
      }
      Some(frame) => {
        if frame.idx < frame.forest.len() {
          break;
        }
        !frame.dotdotdoted ||
          *r.repeat_idx.last().unwrap() == *r.repeat_len.last().unwrap() - 1
      }
    };

    /* done with this set; pop or repeat? */
    if should_pop {
      let prev = r.stack.pop().unwrap();
      match r.stack.last_mut() {
        None => {
          r.cur_tok = token::Eof;
          return ret_val;
        }
        Some(frame) => {
          frame.idx += 1;
        }
      }
      if prev.dotdotdoted {
        r.repeat_idx.pop();
        r.repeat_len.pop();
      }
    } else { /* repeat */
      *r.repeat_idx.last_mut().unwrap() += 1;
      r.stack.last_mut().unwrap().idx = 0;
      if let Some(tk) = r.stack.last().unwrap().sep.clone() {
        r.cur_tok = tk; // repeat same span, I guess
        return ret_val;
      }
    }
  }
  loop { /* because it's easiest, this handles `TokenTree::Delimited` not starting
              with a `TokenTree::Token`, even though it won't happen */
    let t = {
      let frame = r.stack.last().unwrap();
      // FIXME(pcwalton): Bad copy.
      frame.forest.get_tt(frame.idx)
    };
    match t {
      TokenTree::Sequence(sp, seq) => {
        // FIXME(pcwalton): Bad copy.
        match lockstep_iter_size(&TokenTree::Sequence(sp, seq.clone()),
                                 r) {
          LisUnconstrained => {
            panic!(r.sp_diag.span_fatal(
                            sp.clone(), /* blame macro writer */
                            "attempted to repeat an expression \
                             containing no syntax \
                             variables matched as repeating at this depth"));
          }
          LisContradiction(ref msg) => {
            // FIXME #2887 blame macro invoker instead
            panic!(r.sp_diag.span_fatal(sp.clone(), &msg[..]));
          }
          LisConstraint(len, _) => {
            if len == 0 {
              if seq.op == tokenstream::KleeneOp::OneOrMore {
                // FIXME #2887 blame invoker
                panic!(r.sp_diag.span_fatal(sp.clone(),
                                                     "this must repeat at least once"));
              }

              r.stack.last_mut().unwrap().idx += 1;
              return tt_next_token(r);
            }
            r.repeat_len.push(len);
            r.repeat_idx.push(0);
            r.stack.push(TtFrame {
              idx: 0,
              dotdotdoted: true,
              sep: seq.separator.clone(),
              forest: TokenTree::Sequence(sp, seq),
            });
          }
        }
      }
      // TokenTree::Delimited or any token that can be unzipped
      seq @ TokenTree::Delimited(..) => {
        // do not advance the idx yet
        r.stack.push(TtFrame {
          forest: seq,
          idx: 0,
          dotdotdoted: false,
          sep: None
        });
        // if this could be 0-length, we'd need to potentially recur here
      }
      TokenTree::Token(sp, DocComment(name)) if r.desugar_doc_comments => {
        r.stack.push(TtFrame {
          forest: TokenTree::Token(sp, DocComment(name)),
          idx: 0,
          dotdotdoted: false,
          sep: None
        });
      }
      TokenTree::Token(sp, tok) => {
        r.cur_span = sp;
        r.cur_tok = tok;
        r.stack.last_mut().unwrap().idx += 1;
        return ret_val;
      }
    }
  }
}