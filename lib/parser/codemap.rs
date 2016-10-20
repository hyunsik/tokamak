use std::cell::RefCell;
use std::path::{Path,PathBuf};
use std::rc::Rc;

use std::env;
use std::fs;
use std::io::{self, Read};
pub use common::codespan::*;
use errors::CodeMapper;

use ast::Name;


#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug, Copy)]
pub struct Spanned<T> {
  pub node: T,
  pub span: Span,
}

pub fn spanned<T>(lo: BytePos, hi: BytePos, t: T) -> Spanned<T> {
  respan(mk_span(lo, hi), t)
}

pub fn respan<T>(sp: Span, t: T) -> Spanned<T> {
  Spanned {node: t, span: sp}
}

pub fn dummy_spanned<T>(t: T) -> Spanned<T> {
  respan(DUMMY_SPAN, t)
}

/// Build a span that covers the two provided spans.
pub fn combine_spans(sp1: Span, sp2: Span) -> Span {
  if sp1 == DUMMY_SPAN && sp2 == DUMMY_SPAN {
    DUMMY_SPAN
  } else if sp1 == DUMMY_SPAN {
    sp2
  } else if sp2 == DUMMY_SPAN {
    sp1
  } else {
    Span {
      lo: if sp1.lo < sp2.lo { sp1.lo } else { sp2.lo },
      hi: if sp1.hi > sp2.hi { sp1.hi } else { sp2.hi },
    }
  }
}

// _____________________________________________________________________________
// FileMap, MultiByteChar, FileName, FileLines
//

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
  file_loader: Box<FileLoader>
}

impl CodeMap {
  pub fn new() -> CodeMap {
    CodeMap {
      files: RefCell::new(Vec::new()),
      file_loader: Box::new(RealFileLoader)
    }
  }

  pub fn with_file_loader(file_loader: Box<FileLoader>) -> CodeMap {
    CodeMap {
      files: RefCell::new(Vec::new()),
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

  /// Creates a new filemap and sets its line information.
  pub fn new_filemap_and_lines(&self, filename: &str, abs_path: Option<&str>,
                               src: &str) -> Rc<FileMap> {
    let fm = self.new_filemap(filename.to_string(),
                              abs_path.map(|s| s.to_owned()),
                              src.to_owned());
    let mut byte_pos: u32 = fm.start_pos.0;
    for line in src.lines() {
      // register the start of this line
      fm.next_line(BytePos(byte_pos));

      // update byte_pos to include this line and the \n at the end
      byte_pos += line.len() as u32 + 1;
    }
    fm
  }


  /// Allocates a new FileMap representing a source file from an external
  /// crate. The source code of such an "imported filemap" is not available,
  /// but we still know enough to generate accurate debuginfo location
  /// information for things inlined from other crates.
  pub fn new_imported_filemap(&self,
                              filename: FileName,
                              abs_path: Option<FileName>,
                              source_len: usize,
                              mut file_local_lines: Vec<BytePos>,
                              mut file_local_multibyte_chars: Vec<MultiByteChar>)
                              -> Rc<FileMap> {
    let start_pos = self.next_start_pos();
    let mut files = self.files.borrow_mut();

    let end_pos = Pos::from_usize(start_pos + source_len);
    let start_pos = Pos::from_usize(start_pos);

    for pos in &mut file_local_lines {
      *pos = *pos + start_pos;
    }

    for mbc in &mut file_local_multibyte_chars {
      mbc.pos = mbc.pos + start_pos;
    }

    let filemap = Rc::new(FileMap {
      name: filename,
      abs_path: abs_path,
      src: None,
      start_pos: start_pos,
      end_pos: end_pos,
      lines: RefCell::new(file_local_lines),
      multibyte_chars: RefCell::new(file_local_multibyte_chars),
    });

    files.push(filemap.clone());

    filemap
  }

  pub fn mk_substr_filename(&self, sp: Span) -> String {
    let pos = self.lookup_char_pos(sp.lo);
    (format!("<{}:{}:{}>",
             pos.file.name,
             pos.line,
             pos.col.to_usize() + 1)).to_string()
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

    match f.lookup_line(pos) {
      Some(line) => Ok(FileMapAndLine { fm: f, line: line }),
      None => Err(f)
    }
  }

  pub fn lookup_char_pos_adj(&self, pos: BytePos) -> LocWithOpt {
    let loc = self.lookup_char_pos(pos);
    LocWithOpt {
      filename: loc.file.name.to_string(),
      line: loc.line,
      col: loc.col,
      file: Some(loc.file)
    }
  }

  /// Returns `Some(span)`, a union of the lhs and rhs span.  The lhs must precede the rhs. If
  /// there are gaps between lhs and rhs, the resulting union will cross these gaps.
  /// For this to work, the spans have to be:
  ///    * the expn_id of both spans much match
  ///    * the lhs span needs to end on the same line the rhs span begins
  ///    * the lhs span must start at or before the rhs span
  pub fn merge_spans(&self, sp_lhs: Span, sp_rhs: Span) -> Option<Span> {
    use std::cmp;

    let lhs_end = match self.lookup_line(sp_lhs.hi) {
      Ok(x) => x,
      Err(_) => return None
    };
    let rhs_begin = match self.lookup_line(sp_rhs.lo) {
      Ok(x) => x,
      Err(_) => return None
    };

    // if we must cross lines to merge, don't merge
    if lhs_end.line != rhs_begin.line {
      return None;
    }

    // ensure these follow the expected order and we don't overlap
    if (sp_lhs.lo <= sp_rhs.lo) && (sp_lhs.hi <= sp_rhs.lo) {
      Some(Span {
        lo: cmp::min(sp_lhs.lo, sp_rhs.lo),
        hi: cmp::max(sp_lhs.hi, sp_rhs.hi),
      })
    } else {
      None
    }
  }

  pub fn span_to_string(&self, sp: Span) -> String {
    if sp == COMMAND_LINE_SP {
      return "<command line option>".to_string();
    }

    if self.files.borrow().is_empty() && sp.source_equal(&DUMMY_SPAN) {
      return "no-location".to_string();
    }

    let lo = self.lookup_char_pos_adj(sp.lo);
    let hi = self.lookup_char_pos_adj(sp.hi);
    return (format!("{}:{}:{}: {}:{}",
                    lo.filename,
                    lo.line,
                    lo.col.to_usize() + 1,
                    hi.line,
                    hi.col.to_usize() + 1)).to_string()
  }

  pub fn span_to_filename(&self, sp: Span) -> FileName {
    self.lookup_char_pos(sp.lo).file.name.to_string()
  }

  pub fn span_to_lines(&self, sp: Span) -> FileLinesResult {
    debug!("span_to_lines(sp={:?})", sp);

    if sp.lo > sp.hi {
      return Err(SpanLinesError::IllFormedSpan(sp));
    }

    let lo = self.lookup_char_pos(sp.lo);
    debug!("span_to_lines: lo={:?}", lo);
    let hi = self.lookup_char_pos(sp.hi);
    debug!("span_to_lines: hi={:?}", hi);

    if lo.file.start_pos != hi.file.start_pos {
      return Err(SpanLinesError::DistinctSources(DistinctSources {
        begin: (lo.file.name.clone(), lo.file.start_pos),
        end: (hi.file.name.clone(), hi.file.start_pos),
      }));
    }
    assert!(hi.line >= lo.line);

    let mut lines = Vec::with_capacity(hi.line - lo.line + 1);

    // The span starts partway through the first line,
    // but after that it starts from offset 0.
    let mut start_col = lo.col;

    // For every line but the last, it extends from `start_col`
    // and to the end of the line. Be careful because the line
    // numbers in Loc are 1-based, so we subtract 1 to get 0-based
    // lines.
    for line_index in lo.line-1 .. hi.line-1 {
      let line_len = lo.file.get_line(line_index)
        .map(|s| s.chars().count())
        .unwrap_or(0);
      lines.push(LineInfo { line_index: line_index,
        start_col: start_col,
        end_col: CharPos::from_usize(line_len) });
      start_col = CharPos::from_usize(0);
    }

    // For the last line, it extends from `start_col` to `hi.col`:
    lines.push(LineInfo { line_index: hi.line - 1,
      start_col: start_col,
      end_col: hi.col });

    Ok(FileLines {file: lo.file, lines: lines})
  }

  pub fn span_to_snippet(&self, sp: Span) -> Result<String, SpanSnippetError> {
    if sp.lo > sp.hi {
      return Err(SpanSnippetError::IllFormedSpan(sp));
    }

    let local_begin = self.lookup_byte_offset(sp.lo);
    let local_end = self.lookup_byte_offset(sp.hi);

    if local_begin.fm.start_pos != local_end.fm.start_pos {
      return Err(SpanSnippetError::DistinctSources(DistinctSources {
        begin: (local_begin.fm.name.clone(),
                local_begin.fm.start_pos),
        end: (local_end.fm.name.clone(),
              local_end.fm.start_pos)
      }));
    } else {
      match local_begin.fm.src {
        Some(ref src) => {
          let start_index = local_begin.pos.to_usize();
          let end_index = local_end.pos.to_usize();
          let source_len = (local_begin.fm.end_pos -
            local_begin.fm.start_pos).to_usize();

          if start_index > end_index || end_index > source_len {
            return Err(SpanSnippetError::MalformedForCodemap(
              MalformedCodemapPositions {
                name: local_begin.fm.name.clone(),
                source_len: source_len,
                begin_pos: local_begin.pos,
                end_pos: local_end.pos,
              }));
          }

          return Ok((&src[start_index..end_index]).to_string())
        }
        None => {
          return Err(SpanSnippetError::SourceNotAvailable {
            filename: local_begin.fm.name.clone()
          });
        }
      }
    }
  }

  pub fn get_filemap(&self, filename: &str) -> Option<Rc<FileMap>> {
    for fm in self.files.borrow().iter() {
      if filename == fm.name {
        return Some(fm.clone());
      }
    }
    None
  }

  /// For a global BytePos compute the local offset within the containing FileMap
  pub fn lookup_byte_offset(&self, bpos: BytePos) -> FileMapAndBytePos {
    let idx = self.lookup_filemap_idx(bpos);
    let fm = (*self.files.borrow())[idx].clone();
    let offset = bpos - fm.start_pos;
    FileMapAndBytePos {fm: fm, pos: offset}
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
  pub fn lookup_filemap_idx(&self, pos: BytePos) -> usize {
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

  pub fn count_lines(&self) -> usize {
    self.files.borrow().iter().fold(0, |a, f| a + f.count_lines())
  }
}

impl CodeMapper for CodeMap {
  fn lookup_char_pos(&self, pos: BytePos) -> Loc {
    self.lookup_char_pos(pos)
  }
  fn span_to_lines(&self, sp: Span) -> FileLinesResult {
    self.span_to_lines(sp)
  }
  fn span_to_string(&self, sp: Span) -> String {
    self.span_to_string(sp)
  }
  fn span_to_filename(&self, sp: Span) -> FileName {
    self.span_to_filename(sp)
  }
  fn merge_spans(&self, sp_lhs: Span, sp_rhs: Span) -> Option<Span> {
    self.merge_spans(sp_lhs, sp_rhs)
  }
}