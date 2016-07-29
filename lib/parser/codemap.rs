use std::cell::RefCell;
use std::cmp;
use std::env;
use std::{fmt, fs};
use std::io::{self, Read};
use std::ops::{Add, Sub};
use std::path::{Path, PathBuf};
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
// Loc, LocWithOpt, FileMapAndLine, FileMapAndBytePos
//

/// A source code location used for error reporting
#[derive(Debug)]
pub struct Loc {
  /// Information about the original source
  pub file: Rc<FileMap>,
  /// The (1-based) line number
  pub line: usize,
  /// The (0-based) column offset
  pub col: CharPos
}

/// A source code location used as the result of lookup_char_pos_adj
// Actually, *none* of the clients use the filename *or* file field;
// perhaps they should just be removed.
#[derive(Debug)]
pub struct LocWithOpt {
  pub filename: FileName,
  pub line: usize,
  pub col: CharPos,
  pub file: Option<Rc<FileMap>>,
}

// used to be structural records. Better names, anyone?
#[derive(Debug)]
pub struct FileMapAndLine { pub fm: Rc<FileMap>, pub line: usize }
#[derive(Debug)]
pub struct FileMapAndBytePos { pub fm: Rc<FileMap>, pub pos: BytePos }

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
  /// The absolute path of the file that the source came from.
  pub abs_path: Option<FileName>,
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

  /// Return an absolute path to a file, if possible.
  fn abs_path(&self, path: &Path) -> Option<PathBuf>;

  /// Read the contents of an UTF-8 file into memory.
  fn read_file(&self, path: &Path) -> io::Result<String>;
}

/// A FileLoader that uses std::fs to load real files.
pub struct RealFileLoader;

impl FileLoader for RealFileLoader {
  fn file_exists(&self, path: &Path) -> bool {
    fs::metadata(path).is_ok()
  }

  fn abs_path(&self, path: &Path) -> Option<PathBuf> {
    if path.is_absolute() {
      Some(path.to_path_buf())
    } else {
      env::current_dir()
          .ok()
          .map(|cwd| cwd.join(path))
    }
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

  pub fn with_file_loader(file_loader: Box<FileLoader>) -> CodeMap {
    CodeMap {
      files: RefCell::new(Vec::new()),
      expansions: RefCell::new(Vec::new()),
      file_loader: file_loader
    }
  }

  pub fn file_exists(&self, path: &Path) -> bool {
    self.file_loader.file_exists(path)
  }

  pub fn load_file(&self, path: &Path) -> io::Result<Rc<FileMap>> {
    let src = self.file_loader.read_file(path)?;
    let abs_path = self.file_loader.abs_path(path).map(|p| p.to_str().unwrap().to_string());
    Ok(self.new_filemap(path.to_str().unwrap().to_string(), abs_path, src))
  }

  fn next_start_pos(&self) -> usize {
    let files = self.files.borrow();
    match files.last() {
      None => 0,
      // Add one so there is some space between files. This lets us distinguish
      // positions in the codemap, even in the presence of zero-length files.
      Some(last) => last.end_pos.to_usize() + 1,
    }
  }

  /// Creates a new filemap without setting its line information. If you don't
  /// intend to set the line information yourself, you should use new_filemap_and_lines.
  pub fn new_filemap(&self, filename: FileName, abs_path: Option<FileName>,
                     mut src: String) -> Rc<FileMap> {
    let start_pos = self.next_start_pos();
    let mut files = self.files.borrow_mut();

    // Remove utf-8 BOM if any.
    if src.starts_with("\u{feff}") {
      src.drain(..3);
    }

    let end_pos = start_pos + src.len();

    let filemap = Rc::new(FileMap {
      name: filename,
      abs_path: abs_path,
      src: Some(Rc::new(src)),
      start_pos: Pos::from_usize(start_pos),
      end_pos: Pos::from_usize(end_pos),
      lines: RefCell::new(Vec::new()),
      multibyte_chars: RefCell::new(Vec::new()),
    });

    files.push(filemap.clone());

    filemap
  }

  /// Lookup source information about a BytePos
  pub fn lookup_char_pos(&self, pos: BytePos) -> Loc {
    let chpos = self.bytepos_to_file_charpos(pos);
    match self.lookup_line(pos) {
      Ok(FileMapAndLine { fm: f, line: a }) => {
        let line = a + 1; // Line numbers start at 1
        let linebpos = (*f.lines.borrow())[a];
        let linechpos = self.bytepos_to_file_charpos(linebpos);
        debug!("byte pos {:?} is on the line at byte pos {:?}",
               pos, linebpos);
        debug!("char pos {:?} is on the line at char pos {:?}",
               chpos, linechpos);
        debug!("byte is on line: {}", line);
        assert!(chpos >= linechpos);
        Loc {
          file: f,
          line: line,
          col: chpos - linechpos,
        }
      }
      Err(f) => {
        Loc {
          file: f,
          line: 0,
          col: chpos,
        }
      }
    }
  }

  // If the relevant filemap is empty, we don't return a line number.
  fn lookup_line(&self, pos: BytePos) -> Result<FileMapAndLine, Rc<FileMap>> {
    let idx = self.lookup_filemap_idx(pos);

    let files = self.files.borrow();
    let f = (*files)[idx].clone();

    let len = f.lines.borrow().len();
    if len == 0 {
      return Err(f);
    }

    let mut a = 0;
    {
      let lines = f.lines.borrow();
      let mut b = lines.len();
      while b - a > 1 {
        let m = (a + b) / 2;
        if (*lines)[m] > pos {
          b = m;
        } else {
          a = m;
        }
      }
      assert!(a <= lines.len());
    }
    Ok(FileMapAndLine { fm: f, line: a })
  }

  /// Converts an absolute BytePos to a CharPos relative to the filemap.
  pub fn bytepos_to_file_charpos(&self, bpos: BytePos) -> CharPos {
    let idx = self.lookup_filemap_idx(bpos);
    let files = self.files.borrow();
    let map = &(*files)[idx];

    // The number of extra bytes due to multibyte chars in the FileMap
    let mut total_extra_bytes = 0;

    for mbc in map.multibyte_chars.borrow().iter() {
      debug!("{}-byte char at {:?}", mbc.bytes, mbc.pos);
      if mbc.pos < bpos {
        // every character is at least one byte, so we only
        // count the actual extra bytes.
        total_extra_bytes += mbc.bytes - 1;
        // We should never see a byte position in the middle of a
        // character
        assert!(bpos.to_usize() >= mbc.pos.to_usize() + mbc.bytes);
      } else {
        break;
      }
    }

    assert!(map.start_pos.to_usize() + total_extra_bytes <= bpos.to_usize());
    CharPos(bpos.to_usize() - map.start_pos.to_usize() - total_extra_bytes)
  }

  // Return the index of the filemap (in self.files) which contains pos.
  fn lookup_filemap_idx(&self, pos: BytePos) -> usize {
    let files = self.files.borrow();
    let files = &*files;
    let count = files.len();

    // Binary search for the filemap.
    let mut a = 0;
    let mut b = count;
    while b - a > 1 {
      let m = (a + b) / 2;
      if files[m].start_pos > pos {
        b = m;
      } else {
        a = m;
      }
    }

    assert!(a < count, "position {} does not resolve to a source location", pos.to_usize());

    return a;
  }
}