use std::cell::RefCell;
use std::cmp;
use std::{fmt, fs};
use std::io::{self, Read};
use std::ops::{Add, Sub};
use std::path::Path;
use std::rc::Rc;

use ast::Name;

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

pub fn spanned<T>(lo: BytePos, hi: BytePos, t: T) -> Spanned<T> {
  respan(mk_span(lo, hi), t)
}

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

// _____________________________________________________________________________
// ExpnFormat, NameAndSpan, ExpnInfo, ExpnId
//

/// The source of expansion.
#[derive(Clone, Hash, Debug, PartialEq, Eq)]
pub enum ExpnFormat {
  /// e.g. #[derive(...)] <item>
  MacroAttribute(Name),
  /// e.g. `format!()`
  MacroBang(Name),
}

#[derive(Clone, Hash, Debug)]
pub struct NameAndSpan {
  /// The format with which the macro was invoked.
  pub format: ExpnFormat,
  /// Whether the macro is allowed to use #[unstable]/feature-gated
  /// features internally without forcing the whole crate to opt-in
  /// to them.
  pub allow_internal_unstable: bool,
  /// The span of the macro definition itself. The macro may not
  /// have a sensible definition span (e.g. something defined
  /// completely inside libsyntax) in which case this is None.
  pub span: Option<Span>
}

impl NameAndSpan {
  pub fn name(&self) -> Name {
    match self.format {
      ExpnFormat::MacroAttribute(s) => s,
      ExpnFormat::MacroBang(s) => s,
    }
  }
}

/// Extra information for tracking spans of macro and syntax sugar expansion
#[derive(Hash, Debug)]
pub struct ExpnInfo {
  /// The location of the actual macro invocation or syntax sugar , e.g.
  /// `let x = foo!();` or `if let Some(y) = x {}`
  ///
  /// This may recursively refer to other macro invocations, e.g. if
  /// `foo!()` invoked `bar!()` internally, and there was an
  /// expression inside `bar!`; the call_site of the expression in
  /// the expansion would point to the `bar!` invocation; that
  /// call_site span would have its own ExpnInfo, with the call_site
  /// pointing to the `foo!` invocation.
  pub call_site: Span,
  /// Information about the expansion.
  pub callee: NameAndSpan
}

// _____________________________________________________________________________
// FileMap, MultiByteChar, FileName, FileLines
//

pub type FileName = String;

/// Identifies an offset of a multi-byte character in a FileMap
#[derive(Copy, Clone, Eq, PartialEq)]
pub struct MultiByteChar {
  /// The absolute offset of the character in the CodeMap
  pub pos: BytePos,
  /// The number of bytes, >=2
  pub bytes: usize,
}

/// A single source in the CodeMap.
pub struct FileMap {
  /// The name of the file that the source came from, source that doesn't
  /// originate from files has names between angle brackets by convention,
  /// e.g. `<anon>`
  pub name: FileName,
  /// The complete source code
  pub src: Option<Rc<String>>,
  /// The start position of this source in the CodeMap
  pub start_pos: BytePos,
  /// The end position of this source in the CodeMap
  pub end_pos: BytePos,
  /// Locations of lines beginnings in the source code
  pub lines: RefCell<Vec<BytePos>>,
  /// Locations of multi-byte characters in the source code
  pub multibyte_chars: RefCell<Vec<MultiByteChar>>,
}

impl fmt::Debug for FileMap {
  fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
    write!(fmt, "FileMap({})", self.name)
  }
}

/// An abstraction over the fs operations used by the Parser.
pub trait FileLoader {
  /// Query the existence of a file.
  fn file_exists(&self, path: &Path) -> bool;

  /// Read the contents of an UTF-8 file into memory.
  fn read_file(&self, path: &Path) -> io::Result<String>;
}

/// A FileLoader that uses std::fs to load real files.
pub struct RealFileLoader;

impl FileLoader for RealFileLoader {
  fn file_exists(&self, path: &Path) -> bool {
    fs::metadata(path).is_ok()
  }

  fn read_file(&self, path: &Path) -> io::Result<String> {
    let mut src = String::new();
    fs::File::open(path)?.read_to_string(&mut src)?;
    Ok(src)
  }
}

// _____________________________________________________________________________
// CodeMap
//

pub struct CodeMap {
  pub files: RefCell<Vec<Rc<FileMap>>>,
  expansions: RefCell<Vec<ExpnInfo>>,
  file_loader: Box<FileLoader>
}

impl CodeMap {
  pub fn new() -> CodeMap {
    CodeMap {
      files: RefCell::new(Vec::new()),
      expansions: RefCell::new(Vec::new()),
      file_loader: Box::new(RealFileLoader)
    }
  }
}