use std::io::{self, Write};
use std::rc::Rc;

use term;

use common::codespan::{COMMAND_LINE_SP, DUMMY_SPAN, FileMap, MultiSpan, CharPos};

use RenderSpan::*;
use {Level, CodeSuggestion, DiagnosticBuilder, SubDiagnostic, CodeMapper};
use snippet::{StyledString, Style, Annotation, Line};
use styled_buffer::StyledBuffer;

use self::Destination::*;

/// Emitter trait for emitting errors.
pub trait Emitter {
  /// Emit a structured diagnostic.
  fn emit(&mut self, db: &DiagnosticBuilder);
}

impl Emitter for EmitterWriter {
  fn emit(&mut self, db: &DiagnosticBuilder) {
    self.emit_messages_default(&db.level, &db.message, &db.code,
                               &db.span.clone(),
                               &db.children.clone());
  }
}

/// maximum number of lines we will print for each error; arbitrary.
pub const MAX_HIGHLIGHT_LINES: usize = 6;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ColorConfig {
  Auto,
  Always,
  Never,
}

impl ColorConfig {
  fn use_color(&self) -> bool {
    match *self {
      ColorConfig::Always => true,
      ColorConfig::Never  => false,
      ColorConfig::Auto   => stderr_isatty(),
    }
  }
}

pub struct EmitterWriter {
  dst: Destination,
  cm: Option<Rc<CodeMapper>>,
}

struct FileWithAnnotatedLines {
  file: Rc<FileMap>,
  lines: Vec<Line>,
}


/// Do not use this for messages that end in `\n` – use `println_maybe_styled` instead. See
/// `EmitterWriter::print_maybe_styled` for details.
macro_rules! print_maybe_styled {
    ($dst: expr, $style: expr, $($arg: tt)*) => {
        $dst.print_maybe_styled(format_args!($($arg)*), $style, false)
    }
}

macro_rules! println_maybe_styled {
    ($dst: expr, $style: expr, $($arg: tt)*) => {
        $dst.print_maybe_styled(format_args!($($arg)*), $style, true)
    }
}

impl EmitterWriter {
  pub fn stderr(color_config: ColorConfig,
                code_map: Option<Rc<CodeMapper>>)
                -> EmitterWriter {
    if color_config.use_color() {
      let dst = Destination::from_stderr();
      EmitterWriter { dst: dst,
        cm: code_map}
    } else {
      EmitterWriter { dst: Raw(Box::new(io::stderr())),
        cm: code_map}
    }
  }

  pub fn new(dst: Box<Write + Send>,
             code_map: Option<Rc<CodeMapper>>)
             -> EmitterWriter {
    EmitterWriter {
      dst: Raw(dst),
      cm: code_map,
    }
  }

  fn preprocess_annotations(&self, msp: &MultiSpan) -> Vec<FileWithAnnotatedLines> {
    fn add_annotation_to_file(file_vec: &mut Vec<FileWithAnnotatedLines>,
                              file: Rc<FileMap>,
                              line_index: usize,
                              ann: Annotation) {
      for slot in file_vec.iter_mut() {
        // Look through each of our files for the one we're adding to
        if slot.file.name == file.name {
          // See if we already have a line for it
          for line_slot in &mut slot.lines {
            if line_slot.line_index == line_index {
              line_slot.annotations.push(ann);
              return;
            }
          }
          // We don't have a line yet, create one
          slot.lines.push(Line {
            line_index: line_index,
            annotations: vec![ann],
          });
          slot.lines.sort();
          return;
        }
      }
      // This is the first time we're seeing the file
      file_vec.push(FileWithAnnotatedLines {
        file: file,
        lines: vec![Line {
                                line_index: line_index,
                                annotations: vec![ann],
                            }],
      });
    }

    let mut output = vec![];

    if let Some(ref cm) = self.cm {
      for span_label in msp.span_labels() {
        if span_label.span == DUMMY_SPAN || span_label.span == COMMAND_LINE_SP {
          continue;
        }
        let lo = cm.lookup_char_pos(span_label.span.lo);
        let mut hi = cm.lookup_char_pos(span_label.span.hi);
        let mut is_minimized = false;

        // If the span is multi-line, simplify down to the span of one character
        if lo.line != hi.line {
          hi.line = lo.line;
          hi.col = CharPos(lo.col.0 + 1);
          is_minimized = true;
        }

        // Watch out for "empty spans". If we get a span like 6..6, we
        // want to just display a `^` at 6, so convert that to
        // 6..7. This is degenerate input, but it's best to degrade
        // gracefully -- and the parser likes to supply a span like
        // that for EOF, in particular.
        if lo.col == hi.col {
          hi.col = CharPos(lo.col.0 + 1);
        }

        add_annotation_to_file(&mut output,
                               lo.file,
                               lo.line,
                               Annotation {
                                 start_col: lo.col.0,
                                 end_col: hi.col.0,
                                 is_primary: span_label.is_primary,
                                 is_minimized: is_minimized,
                                 label: span_label.label.clone(),
                               });
      }
    }

    output
  }

  fn render_source_line(&self,
                        buffer: &mut StyledBuffer,
                        file: Rc<FileMap>,
                        line: &Line,
                        width_offset: usize) {
    let source_string = file.get_line(line.line_index - 1)
      .unwrap_or("");

    let line_offset = buffer.num_lines();

    // First create the source line we will highlight.
    buffer.puts(line_offset, width_offset, &source_string, Style::Quotation);
    buffer.puts(line_offset,
                0,
                &(line.line_index.to_string()),
                Style::LineNumber);

    draw_col_separator(buffer, line_offset, width_offset - 2);

    if line.annotations.is_empty() {
      return;
    }

    // We want to display like this:
    //
    //      vec.push(vec.pop().unwrap());
    //      ---      ^^^               _ previous borrow ends here
    //      |        |
    //      |        error occurs here
    //      previous borrow of `vec` occurs here
    //
    // But there are some weird edge cases to be aware of:
    //
    //      vec.push(vec.pop().unwrap());
    //      --------                    - previous borrow ends here
    //      ||
    //      |this makes no sense
    //      previous borrow of `vec` occurs here
    //
    // For this reason, we group the lines into "highlight lines"
    // and "annotations lines", where the highlight lines have the `~`.

    // Sort the annotations by (start, end col)
    let mut annotations = line.annotations.clone();
    annotations.sort();

    // Next, create the highlight line.
    for annotation in &annotations {
      for p in annotation.start_col..annotation.end_col {
        if annotation.is_primary {
          buffer.putc(line_offset + 1,
                      width_offset + p,
                      '^',
                      Style::UnderlinePrimary);
          if !annotation.is_minimized {
            buffer.set_style(line_offset,
                             width_offset + p,
                             Style::UnderlinePrimary);
          }
        } else {
          buffer.putc(line_offset + 1,
                      width_offset + p,
                      '-',
                      Style::UnderlineSecondary);
          if !annotation.is_minimized {
            buffer.set_style(line_offset,
                             width_offset + p,
                             Style::UnderlineSecondary);
          }
        }
      }
    }
    draw_col_separator(buffer, line_offset + 1, width_offset - 2);

    // Now we are going to write labels in. To start, we'll exclude
    // the annotations with no labels.
    let (labeled_annotations, unlabeled_annotations): (Vec<_>, _) = annotations.into_iter()
      .partition(|a| a.label.is_some());

    // If there are no annotations that need text, we're done.
    if labeled_annotations.is_empty() {
      return;
    }
    // Now add the text labels. We try, when possible, to stick the rightmost
    // annotation at the end of the highlight line:
    //
    //      vec.push(vec.pop().unwrap());
    //      ---      ---               - previous borrow ends here
    //
    // But sometimes that's not possible because one of the other
    // annotations overlaps it. For example, from the test
    // `span_overlap_label`, we have the following annotations
    // (written on distinct lines for clarity):
    //
    //      fn foo(x: u32) {
    //      --------------
    //             -
    //
    // In this case, we can't stick the rightmost-most label on
    // the highlight line, or we would get:
    //
    //      fn foo(x: u32) {
    //      -------- x_span
    //      |
    //      fn_span
    //
    // which is totally weird. Instead we want:
    //
    //      fn foo(x: u32) {
    //      --------------
    //      |      |
    //      |      x_span
    //      fn_span
    //
    // which is...less weird, at least. In fact, in general, if
    // the rightmost span overlaps with any other span, we should
    // use the "hang below" version, so we can at least make it
    // clear where the span *starts*.
    let mut labeled_annotations = &labeled_annotations[..];
    match labeled_annotations.split_last().unwrap() {
      (last, previous) => {
        if previous.iter()
          .chain(&unlabeled_annotations)
          .all(|a| !overlaps(a, last)) {
          // append the label afterwards; we keep it in a separate
          // string
          let highlight_label: String = format!(" {}", last.label.as_ref().unwrap());
          if last.is_primary {
            buffer.append(line_offset + 1, &highlight_label, Style::LabelPrimary);
          } else {
            buffer.append(line_offset + 1, &highlight_label, Style::LabelSecondary);
          }
          labeled_annotations = previous;
        }
      }
    }

    // If that's the last annotation, we're done
    if labeled_annotations.is_empty() {
      return;
    }

    for (index, annotation) in labeled_annotations.iter().enumerate() {
      // Leave:
      // - 1 extra line
      // - One line for each thing that comes after
      let comes_after = labeled_annotations.len() - index - 1;
      let blank_lines = 3 + comes_after;

      // For each blank line, draw a `|` at our column. The
      // text ought to be long enough for this.
      for index in 2..blank_lines {
        if annotation.is_primary {
          buffer.putc(line_offset + index,
                      width_offset + annotation.start_col,
                      '|',
                      Style::UnderlinePrimary);
        } else {
          buffer.putc(line_offset + index,
                      width_offset + annotation.start_col,
                      '|',
                      Style::UnderlineSecondary);
        }
        draw_col_separator(buffer, line_offset + index, width_offset - 2);
      }

      if annotation.is_primary {
        buffer.puts(line_offset + blank_lines,
                    width_offset + annotation.start_col,
                    annotation.label.as_ref().unwrap(),
                    Style::LabelPrimary);
      } else {
        buffer.puts(line_offset + blank_lines,
                    width_offset + annotation.start_col,
                    annotation.label.as_ref().unwrap(),
                    Style::LabelSecondary);
      }
      draw_col_separator(buffer, line_offset + blank_lines, width_offset - 2);
    }
  }

  fn get_multispan_max_line_num(&mut self, msp: &MultiSpan) -> usize {
    let mut max = 0;
    if let Some(ref cm) = self.cm {
      for primary_span in msp.primary_spans() {
        if primary_span != &DUMMY_SPAN && primary_span != &COMMAND_LINE_SP {
          let hi = cm.lookup_char_pos(primary_span.hi);
          if hi.line > max {
            max = hi.line;
          }
        }
      }
      for span_label in msp.span_labels() {
        if span_label.span != DUMMY_SPAN && span_label.span != COMMAND_LINE_SP {
          let hi = cm.lookup_char_pos(span_label.span.hi);
          if hi.line > max {
            max = hi.line;
          }
        }
      }
    }
    max
  }

  fn get_max_line_num(&mut self, span: &MultiSpan, children: &Vec<SubDiagnostic>) -> usize {
    let mut max = 0;

    let primary = self.get_multispan_max_line_num(span);
    max = if primary > max { primary } else { max };

    for sub in children {
      let sub_result = self.get_multispan_max_line_num(&sub.span);
      max = if sub_result > max { primary } else { max };
    }
    max
  }

  fn emit_message_default(&mut self,
                          msp: &MultiSpan,
                          msg: &str,
                          code: &Option<String>,
                          level: &Level,
                          max_line_num_len: usize,
                          is_secondary: bool)
                          -> io::Result<()> {
    let mut buffer = StyledBuffer::new();

    if msp.primary_spans().is_empty() && msp.span_labels().is_empty() && is_secondary {
      // This is a secondary message with no span info
      for _ in 0..max_line_num_len {
        buffer.prepend(0, " ", Style::NoStyle);
      }
      draw_note_separator(&mut buffer, 0, max_line_num_len + 1);
      buffer.append(0, &level.to_string(), Style::HeaderMsg);
      buffer.append(0, ": ", Style::NoStyle);
      buffer.append(0, msg, Style::NoStyle);
    }
      else {
        buffer.append(0, &level.to_string(), Style::Level(level.clone()));
        match code {
          &Some(ref code) => {
            buffer.append(0, "[", Style::Level(level.clone()));
            buffer.append(0, &code, Style::Level(level.clone()));
            buffer.append(0, "]", Style::Level(level.clone()));
          }
          _ => {}
        }
        buffer.append(0, ": ", Style::HeaderMsg);
        buffer.append(0, msg, Style::HeaderMsg);
      }

    // Preprocess all the annotations so that they are grouped by file and by line number
    // This helps us quickly iterate over the whole message (including secondary file spans)
    let mut annotated_files = self.preprocess_annotations(msp);

    // Make sure our primary file comes first
    let primary_lo =
    if let (Some(ref cm), Some(ref primary_span)) = (self.cm.as_ref(),
                                                     msp.primary_span().as_ref()) {
      if primary_span != &&DUMMY_SPAN && primary_span != &&COMMAND_LINE_SP {
        cm.lookup_char_pos(primary_span.lo)
      }
        else {
          emit_to_destination(&buffer.render(), level, &mut self.dst)?;
          return Ok(());
        }
    } else {
      // If we don't have span information, emit and exit
      emit_to_destination(&buffer.render(), level, &mut self.dst)?;
      return Ok(());
    };
    if let Ok(pos) =
    annotated_files.binary_search_by(|x| x.file.name.cmp(&primary_lo.file.name)) {
      annotated_files.swap(0, pos);
    }

    // Print out the annotate source lines that correspond with the error
    for annotated_file in annotated_files {
      // print out the span location and spacer before we print the annotated source
      // to do this, we need to know if this span will be primary
      let is_primary = primary_lo.file.name == annotated_file.file.name;
      if is_primary {
        // remember where we are in the output buffer for easy reference
        let buffer_msg_line_offset = buffer.num_lines();

        buffer.prepend(buffer_msg_line_offset, "--> ", Style::LineNumber);
        let loc = primary_lo.clone();
        buffer.append(buffer_msg_line_offset,
                      &format!("{}:{}:{}", loc.file.name, loc.line, loc.col.0 + 1),
                      Style::LineAndColumn);
        for _ in 0..max_line_num_len {
          buffer.prepend(buffer_msg_line_offset, " ", Style::NoStyle);
        }
      } else {
        // remember where we are in the output buffer for easy reference
        let buffer_msg_line_offset = buffer.num_lines();

        // Add spacing line
        draw_col_separator(&mut buffer, buffer_msg_line_offset, max_line_num_len + 1);

        // Then, the secondary file indicator
        buffer.prepend(buffer_msg_line_offset + 1, "::: ", Style::LineNumber);
        buffer.append(buffer_msg_line_offset + 1,
                      &annotated_file.file.name,
                      Style::LineAndColumn);
        for _ in 0..max_line_num_len {
          buffer.prepend(buffer_msg_line_offset + 1, " ", Style::NoStyle);
        }
      }

      // Put in the spacer between the location and annotated source
      let buffer_msg_line_offset = buffer.num_lines();
      draw_col_separator_no_space(&mut buffer, buffer_msg_line_offset, max_line_num_len + 1);

      // Next, output the annotate source for this file
      for line_idx in 0..annotated_file.lines.len() {
        self.render_source_line(&mut buffer,
                                annotated_file.file.clone(),
                                &annotated_file.lines[line_idx],
                                3 + max_line_num_len);

        // check to see if we need to print out or elide lines that come between
        // this annotated line and the next one
        if line_idx < (annotated_file.lines.len() - 1) {
          let line_idx_delta = annotated_file.lines[line_idx + 1].line_index -
            annotated_file.lines[line_idx].line_index;
          if line_idx_delta > 2 {
            let last_buffer_line_num = buffer.num_lines();
            buffer.puts(last_buffer_line_num, 0, "...", Style::LineNumber);
          } else if line_idx_delta == 2 {
            let unannotated_line = annotated_file.file
              .get_line(annotated_file.lines[line_idx].line_index)
              .unwrap_or("");

            let last_buffer_line_num = buffer.num_lines();

            buffer.puts(last_buffer_line_num,
                        0,
                        &(annotated_file.lines[line_idx + 1].line_index - 1)
                          .to_string(),
                        Style::LineNumber);
            draw_col_separator(&mut buffer, last_buffer_line_num, 1 + max_line_num_len);
            buffer.puts(last_buffer_line_num,
                        3 + max_line_num_len,
                        &unannotated_line,
                        Style::Quotation);
          }
        }
      }
    }

    // final step: take our styled buffer, render it, then output it
    emit_to_destination(&buffer.render(), level, &mut self.dst)?;

    Ok(())
  }
  fn emit_suggestion_default(&mut self,
                             suggestion: &CodeSuggestion,
                             level: &Level,
                             msg: &str,
                             max_line_num_len: usize)
                             -> io::Result<()> {
    use std::borrow::Borrow;

    let primary_span = suggestion.msp.primary_span().unwrap();
    if let Some(ref cm) = self.cm {
      let mut buffer = StyledBuffer::new();

      buffer.append(0, &level.to_string(), Style::Level(level.clone()));
      buffer.append(0, ": ", Style::HeaderMsg);
      buffer.append(0, msg, Style::HeaderMsg);

      let lines = cm.span_to_lines(primary_span).unwrap();

      assert!(!lines.lines.is_empty());

      let complete = suggestion.splice_lines(cm.borrow());

      // print the suggestion without any line numbers, but leave
      // space for them. This helps with lining up with previous
      // snippets from the actual error being reported.
      let mut lines = complete.lines();
      let mut row_num = 1;
      for line in lines.by_ref().take(MAX_HIGHLIGHT_LINES) {
        draw_col_separator(&mut buffer, row_num, max_line_num_len + 1);
        buffer.append(row_num, line, Style::NoStyle);
        row_num += 1;
      }

      // if we elided some lines, add an ellipsis
      if let Some(_) = lines.next() {
        buffer.append(row_num, "...", Style::NoStyle);
      }
      emit_to_destination(&buffer.render(), level, &mut self.dst)?;
    }
    Ok(())
  }
  fn emit_messages_default(&mut self,
                           level: &Level,
                           message: &String,
                           code: &Option<String>,
                           span: &MultiSpan,
                           children: &Vec<SubDiagnostic>) {
    let max_line_num = self.get_max_line_num(span, children);
    let max_line_num_len = max_line_num.to_string().len();

    match self.emit_message_default(span,
                                    message,
                                    code,
                                    level,
                                    max_line_num_len,
                                    false) {
      Ok(()) => {
        if !children.is_empty() {
          let mut buffer = StyledBuffer::new();
          draw_col_separator_no_space(&mut buffer, 0, max_line_num_len + 1);
          match emit_to_destination(&buffer.render(), level, &mut self.dst) {
            Ok(()) => (),
            Err(e) => panic!("failed to emit error: {}", e)
          }
        }
        for child in children {
          match child.render_span {
            Some(FullSpan(ref msp)) => {
              match self.emit_message_default(msp,
                                              &child.message,
                                              &None,
                                              &child.level,
                                              max_line_num_len,
                                              true) {
                Err(e) => panic!("failed to emit error: {}", e),
                _ => ()
              }
            },
            Some(Suggestion(ref cs)) => {
              match self.emit_suggestion_default(cs,
                                                 &child.level,
                                                 &child.message,
                                                 max_line_num_len) {
                Err(e) => panic!("failed to emit error: {}", e),
                _ => ()
              }
            },
            None => {
              match self.emit_message_default(&child.span,
                                              &child.message,
                                              &None,
                                              &child.level,
                                              max_line_num_len,
                                              true) {
                Err(e) => panic!("failed to emit error: {}", e),
                _ => ()
              }
            }
          }
        }
      }
      Err(e) => panic!("failed to emit error: {}", e)
    }
    match write!(&mut self.dst, "\n") {
      Err(e) => panic!("failed to emit error: {}", e),
      _ => match self.dst.flush() {
        Err(e) => panic!("failed to emit error: {}", e),
        _ => ()
      }
    }
  }
}

fn draw_col_separator(buffer: &mut StyledBuffer, line: usize, col: usize) {
  buffer.puts(line, col, "| ", Style::LineNumber);
}

fn draw_col_separator_no_space(buffer: &mut StyledBuffer, line: usize, col: usize) {
  buffer.puts(line, col, "|", Style::LineNumber);
}

fn draw_note_separator(buffer: &mut StyledBuffer, line: usize, col: usize) {
  buffer.puts(line, col, "= ", Style::LineNumber);
}

fn overlaps(a1: &Annotation, a2: &Annotation) -> bool {
  (a2.start_col..a2.end_col).contains(a1.start_col) ||
    (a1.start_col..a1.end_col).contains(a2.start_col)
}

fn emit_to_destination(rendered_buffer: &Vec<Vec<StyledString>>,
                       lvl: &Level,
                       dst: &mut Destination) -> io::Result<()> {
  use lock;

  // In order to prevent error message interleaving, where multiple error lines get intermixed
  // when multiple compiler processes error simultaneously, we emit errors with additional
  // steps.
  //
  // On Unix systems, we write into a buffered terminal rather than directly to a terminal. When
  // the .flush() is called we take the buffer created from the buffered writes and write it at
  // one shot.  Because the Unix systems use ANSI for the colors, which is a text-based styling
  // scheme, this buffered approach works and maintains the styling.
  //
  // On Windows, styling happens through calls to a terminal API. This prevents us from using the
  // same buffering approach.  Instead, we use a global Windows mutex, which we acquire long
  // enough to output the full error message, then we release.
  let _buffer_lock = lock::acquire_global_lock("rustc_errors");
  for line in rendered_buffer {
    for part in line {
      dst.apply_style(lvl.clone(), part.style)?;
      write!(dst, "{}", part.text)?;
      dst.reset_attrs()?;
    }
    write!(dst, "\n")?;
  }
  dst.flush()?;
  Ok(())
}

#[cfg(unix)]
fn stderr_isatty() -> bool {
  use libc;
  unsafe { libc::isatty(libc::STDERR_FILENO) != 0 }
}
#[cfg(windows)]
fn stderr_isatty() -> bool {
  type DWORD = u32;
  type BOOL = i32;
  type HANDLE = *mut u8;
  const STD_ERROR_HANDLE: DWORD = -12i32 as DWORD;
  extern "system" {
    fn GetStdHandle(which: DWORD) -> HANDLE;
    fn GetConsoleMode(hConsoleHandle: HANDLE,
                      lpMode: *mut DWORD) -> BOOL;
  }
  unsafe {
    let handle = GetStdHandle(STD_ERROR_HANDLE);
    let mut out = 0;
    GetConsoleMode(handle, &mut out) != 0
  }
}

pub type BufferedStderr = term::Terminal<Output = BufferedWriter> + Send;

pub enum Destination {
  Terminal(Box<term::StderrTerminal>),
  BufferedTerminal(Box<BufferedStderr>),
  Raw(Box<Write + Send>),
}

/// Buffered writer gives us a way on Unix to buffer up an entire error message before we output
/// it.  This helps to prevent interleaving of multiple error messages when multiple compiler
/// processes error simultaneously
pub struct BufferedWriter {
  buffer: Vec<u8>,
}

impl BufferedWriter {
  // note: we use _new because the conditional compilation at its use site may make this
  // this function unused on some platforms
  fn _new() -> BufferedWriter {
    BufferedWriter {
      buffer: vec![]
    }
  }
}

impl Write for BufferedWriter {
  fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
    for b in buf {
      self.buffer.push(*b);
    }
    Ok(buf.len())
  }
  fn flush(&mut self) -> io::Result<()> {
    let mut stderr = io::stderr();
    let result = (|| {
      stderr.write_all(&self.buffer)?;
      stderr.flush()
    })();
    self.buffer.clear();
    result
  }
}

impl Destination {
  #[cfg(not(windows))]
  /// When not on Windows, prefer the buffered terminal so that we can buffer an entire error
  /// to be emitted at one time.
  fn from_stderr() -> Destination {
    let stderr: Option<Box<BufferedStderr>>  =
    term::TerminfoTerminal::new(BufferedWriter::_new())
      .map(|t| Box::new(t) as Box<BufferedStderr>);

    match stderr {
      Some(t) => BufferedTerminal(t),
      None    => Raw(Box::new(io::stderr())),
    }
  }

  #[cfg(windows)]
  /// Return a normal, unbuffered terminal when on Windows.
  fn from_stderr() -> Destination {
    let stderr: Option<Box<term::StderrTerminal>> =
    term::TerminfoTerminal::new(io::stderr())
      .map(|t| Box::new(t) as Box<term::StderrTerminal>)
      .or_else(|| term::WinConsole::new(io::stderr()).ok()
        .map(|t| Box::new(t) as Box<term::StderrTerminal>));

    match stderr {
      Some(t) => Terminal(t),
      None    => Raw(Box::new(io::stderr())),
    }
  }

  fn apply_style(&mut self,
                 lvl: Level,
                 style: Style)
                 -> io::Result<()> {
    match style {
      Style::FileNameStyle | Style::LineAndColumn => {}
      Style::LineNumber => {
        self.start_attr(term::Attr::Bold)?;
        if cfg!(windows) {
          self.start_attr(term::Attr::ForegroundColor(term::color::BRIGHT_CYAN))?;
        } else {
          self.start_attr(term::Attr::ForegroundColor(term::color::BRIGHT_BLUE))?;
        }
      }
      Style::ErrorCode => {
        self.start_attr(term::Attr::Bold)?;
        self.start_attr(term::Attr::ForegroundColor(term::color::BRIGHT_MAGENTA))?;
      }
      Style::Quotation => {}
      Style::OldSchoolNote => {
        self.start_attr(term::Attr::Bold)?;
        self.start_attr(term::Attr::ForegroundColor(term::color::BRIGHT_GREEN))?;
      }
      Style::OldSchoolNoteText | Style::HeaderMsg => {
        self.start_attr(term::Attr::Bold)?;
        if cfg!(windows) {
          self.start_attr(term::Attr::ForegroundColor(term::color::BRIGHT_WHITE))?;
        }
      }
      Style::UnderlinePrimary | Style::LabelPrimary => {
        self.start_attr(term::Attr::Bold)?;
        self.start_attr(term::Attr::ForegroundColor(lvl.color()))?;
      }
      Style::UnderlineSecondary |
      Style::LabelSecondary => {
        self.start_attr(term::Attr::Bold)?;
        if cfg!(windows) {
          self.start_attr(term::Attr::ForegroundColor(term::color::BRIGHT_CYAN))?;
        } else {
          self.start_attr(term::Attr::ForegroundColor(term::color::BRIGHT_BLUE))?;
        }
      }
      Style::NoStyle => {}
      Style::Level(l) => {
        self.start_attr(term::Attr::Bold)?;
        self.start_attr(term::Attr::ForegroundColor(l.color()))?;
      }
    }
    Ok(())
  }

  fn start_attr(&mut self, attr: term::Attr) -> io::Result<()> {
    match *self {
      Terminal(ref mut t) => { t.attr(attr)?; }
      BufferedTerminal(ref mut t) => { t.attr(attr)?; }
      Raw(_) => { }
    }
    Ok(())
  }

  fn reset_attrs(&mut self) -> io::Result<()> {
    match *self {
      Terminal(ref mut t) => { t.reset()?; }
      BufferedTerminal(ref mut t) => { t.reset()?; }
      Raw(_) => { }
    }
    Ok(())
  }
}

impl Write for Destination {
  fn write(&mut self, bytes: &[u8]) -> io::Result<usize> {
    match *self {
      Terminal(ref mut t) => t.write(bytes),
      BufferedTerminal(ref mut t) => t.write(bytes),
      Raw(ref mut w) => w.write(bytes),
    }
  }
  fn flush(&mut self) -> io::Result<()> {
    match *self {
      Terminal(ref mut t) => t.flush(),
      BufferedTerminal(ref mut t) => t.flush(),
      Raw(ref mut w) => w.flush(),
    }
  }
}