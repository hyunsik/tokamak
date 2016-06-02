use std::cmp;
use std::ops::{Add, Sub};

pub trait Pos {
  fn from_usize(n: usize) -> Self;
  fn to_usize(&self) -> usize;
}

/// A byte offset. Keep this small (currently 32-bits), as AST contains
/// a lot of them.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct BytePos(pub u32);

/// A character offset. Because of multibyte utf8 characters, a byte offset
/// is not equivalent to a character offset. The CodeMap will convert BytePos
/// values to CharPos values as necessary.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Debug)]
pub struct CharPos(pub usize);

impl Pos for BytePos {
  fn from_usize(n: usize) -> BytePos { BytePos(n as u32) }
  fn to_usize(&self) -> usize { let BytePos(n) = *self; n as usize }
}

impl Add for BytePos {
  type Output = BytePos;

  fn add(self, rhs: BytePos) -> BytePos {
    BytePos((self.to_usize() + rhs.to_usize()) as u32)
  }
}

impl Sub for BytePos {
  type Output = BytePos;

  fn sub(self, rhs: BytePos) -> BytePos {
    BytePos((self.to_usize() - rhs.to_usize()) as u32)
  }
}

impl Pos for CharPos {
  fn from_usize(n: usize) -> CharPos { CharPos(n) }
  fn to_usize(&self) -> usize { let CharPos(n) = *self; n }
}

impl Add for CharPos {
  type Output = CharPos;

  fn add(self, rhs: CharPos) -> CharPos {
    CharPos(self.to_usize() + rhs.to_usize())
  }
}

impl Sub for CharPos {
  type Output = CharPos;

  fn sub(self, rhs: CharPos) -> CharPos {
    CharPos(self.to_usize() - rhs.to_usize())
  }
}

/// Spans represent a region of code, used for error reporting. Positions in spans
/// are *absolute* positions from the beginning of the codemap.
#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub struct Span {
  pub lo: BytePos,
  pub hi: BytePos
}

pub const DUMMY_SPAN: Span = Span { lo: BytePos(0), hi: BytePos(0) };

pub fn respan<T>(sp: Span, t: T) -> Spanned<T> {
  Spanned {node: t, span: sp}
}

/* assuming that we're not in macro expansion */
pub fn mk_span(lo: BytePos, hi: BytePos) -> Span {
  Span {lo: lo, hi: hi}
}

impl Span {
  /// Returns a new span representing just the end-point of this span
  pub fn end_point(self) -> Span {
    let lo = cmp::max(self.hi.0 - 1, self.lo.0);
    Span { lo: BytePos(lo), hi: self.hi }
  }

  /// Returns `self` if `self` is not the dummy span, and `other` otherwise.
  pub fn substitute_dummy(self, other: Span) -> Span {
    if self.source_equal(&DUMMY_SPAN) { other } else { self }
  }

  pub fn contains(self, other: Span) -> bool {
    self.lo <= other.lo && other.hi <= self.hi
  }

  /// Return true if the spans are equal with regards to the source text.
  ///
  /// Use this instead of `==` when either span could be generated code,
  /// and you only care that they point to the same bytes of source text.
  pub fn source_equal(&self, other: &Span) -> bool {
    self.lo == other.lo && self.hi == other.hi
  }

  /// Returns `Some(span)`, a union of `self` and `other`, on overlap.
  pub fn merge(self, other: Span) -> Option<Span> {
    if (self.lo <= other.lo && self.hi > other.lo) ||
    (self.lo >= other.lo && self.lo < other.hi) {
      Some(Span {
        lo: cmp::min(self.lo, other.lo),
        hi: cmp::max(self.hi, other.hi),
      })
    } else {
      None
    }
  }

  /// Returns `Some(span)`, where the start is trimmed by the end of `other`
  pub fn trim_start(self, other: Span) -> Option<Span> {
    if self.hi > other.hi {
      Some(Span { lo: cmp::max(self.lo, other.hi), .. self })
    } else {
      None
    }
  }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub struct Spanned<T> {
  pub node: T,
  pub span: Span,
}

/// A collection of spans. Spans have two orthogonal attributes:
///
/// - they can be *primary spans*. In this case they are the locus of
///   the error, and would be rendered with `^^^`.
/// - they can have a *label*. In this case, the label is written next
///   to the mark in the snippet when we render.
#[derive(Clone)]
pub struct MultiSpan {
  primary_spans: Vec<Span>,
  span_labels: Vec<(Span, String)>,
}

#[derive(Clone, Debug)]
pub struct SpanLabel {
  /// The span we are going to include in the final snippet.
  pub span: Span,

  /// Is this a primary span? This is the "locus" of the message,
  /// and is indicated with a `^^^^` underline, versus `----`.
  pub is_primary: bool,

  /// What label should we attach to this span (if any)?
  pub label: Option<String>,
}


impl MultiSpan {
  pub fn new() -> MultiSpan {
    MultiSpan {
      primary_spans: vec![],
      span_labels: vec![]
    }
  }

  pub fn from_span(primary_span: Span) -> MultiSpan {
    MultiSpan {
      primary_spans: vec![primary_span],
      span_labels: vec![]
    }
  }

  pub fn from_spans(vec: Vec<Span>) -> MultiSpan {
    MultiSpan {
      primary_spans: vec,
      span_labels: vec![]
    }
  }

  pub fn push_span_label(&mut self, span: Span, label: String) {
    self.span_labels.push((span, label));
  }

  /// Selects the first primary span (if any)
  pub fn primary_span(&self) -> Option<Span> {
    self.primary_spans.first().cloned()
  }

  /// Returns all primary spans.
  pub fn primary_spans(&self) -> &[Span] {
    &self.primary_spans
  }

  /// Returns the strings to highlight. We always ensure that there
  /// is an entry for each of the primary spans -- for each primary
  /// span P, if there is at least one label with span P, we return
  /// those labels (marked as primary). But otherwise we return
  /// `SpanLabel` instances with empty labels.
  pub fn span_labels(&self) -> Vec<SpanLabel> {
    let is_primary = |span| self.primary_spans.contains(&span);
    let mut span_labels = vec![];

    for &(span, ref label) in &self.span_labels {
      span_labels.push(SpanLabel {
        span: span,
        is_primary: is_primary(span),
        label: Some(label.clone())
      });
    }

    for &span in &self.primary_spans {
      if !span_labels.iter().any(|sl| sl.span == span) {
        span_labels.push(SpanLabel {
          span: span,
          is_primary: true,
          label: None
        });
      }
    }

    span_labels
  }
}

impl From<Span> for MultiSpan {
  fn from(span: Span) -> MultiSpan {
    MultiSpan::from_span(span)
  }
}