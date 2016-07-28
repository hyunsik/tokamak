//! This pretty-printer is a direct reimplementation of Philip Karlton's
//! Mesa pretty-printer, as described in appendix A of
//!
//! ````ignore
//! STAN-CS-79-770: "Pretty Printing", by Derek C. Oppen.
//! Stanford Department of Computer Science, 1979.
//! ````
//!
//! The algorithm's aim is to break a stream into as few lines as possible
//! while respecting the indentation-consistency requirements of the enclosing
//! block, and avoiding breaking at silly places on block boundaries, for
//! example, between "x" and ")" in "x)".
//!
//! I am implementing this algorithm because it comes with 20 pages of
//! documentation explaining its theory, and because it addresses the set of
//! concerns I've seen other pretty-printers fall down on. Weirdly. Even though
//! it's 32 years old. What can I say?
//!
//! Despite some redundancies and quirks in the way it's implemented in that
//! paper, I've opted to keep the implementation here as similar as I can,
//! changing only what was blatantly wrong, a typo, or sufficiently
//! non-idiomatic rust that it really stuck out.
//!
//! In particular you'll see a certain amount of churn related to INTEGER vs.
//! CARDINAL in the Mesa implementation. Mesa apparently interconverts the two
//! somewhat readily? In any case, I've used usize for indices-in-buffers and
//! ints for character-sizes-and-indentation-offsets. This respects the need
//! for ints to "go negative" while carrying a pending-calculation balance, and
//! helps differentiate all the numbers flying around internally (slightly).
//!
//! I also inverted the indentation arithmetic used in the print stack, since
//! the Mesa implementation (somewhat randomly) stores the offset on the print
//! stack in terms of margin-col rather than col itself. I store col.
//!
//! I also implemented a small change in the String token, in that I store an
//! explicit length for the string. For most tokens this is just the length of
//! the accompanying string. But it's necessary to permit it to differ, for
//! encoding things that are supposed to "go on their own line" -- certain
//! classes of comment and blank-line -- where relying on adjacent
//! hardbreak-like Break tokens with long blankness indication doesn't actually
//! work. To see why, consider when there is a "thing that should be on its own
//! line" between two long blocks, say functions. If you put a hardbreak after
//! each function (or before each) and the breaking algorithm decides to break
//! there anyways (because the functions themselves are long) you wind up with
//! extra blank lines. If you don't put hardbreaks you can wind up with the
//! "thing which should be on its own line" not getting its own line in the
//! rare case of "really small functions" or such. This re-occurs with comments
//! and explicit blank lines. So in those cases we use a string with a payload
//! we want isolated to a line and an explicit length that's huge, surrounded
//! by two zero-length breaks. The algorithm will try its best to fit it on a
//! line (which it can't) and so naturally place the content on its own line to
//! avoid combining it with other lines and making matters even worse.

use std::ascii;
use std::io::{self, Write, Read};
use std::iter;
use std::string;

use itertools::Itertools;

use self::AnnNode::*;
use self::Breaks::*;

use abi::Abi;
use ast::{self, Attribute, BlockCheckMode, Mutability, PatKind};
use codemap::{self, CodeMap, BytePos};
use comments;
use error_handler as errors;
use parser;
use precedence::AssocOp;
use ptr::P;
use token::keywords;

#[derive(Clone, Copy, PartialEq)]
pub enum Breaks {
  Consistent,
  Inconsistent,
}

#[derive(Clone, Copy)]
pub struct BreakToken {
  offset: isize,
  blank_space: isize
}

#[derive(Clone, Copy)]
pub struct BeginToken {
  offset: isize,
  breaks: Breaks
}

#[derive(Clone)]
pub enum Token {
  String(String, isize),
  Break(BreakToken),
  Begin(BeginToken),
  End,
  Eof,
}

impl Token {
  pub fn is_eof(&self) -> bool {
    match *self {
      Token::Eof => true,
      _ => false,
    }
  }

  pub fn is_hardbreak_tok(&self) -> bool {
    match *self {
      Token::Break(BreakToken {
                     offset: 0,
                     blank_space: bs
                   }) if bs == SIZE_INFINITY =>
        true,
      _ =>
        false
    }
  }
}


pub fn tok_str(token: &Token) -> String {
  match *token {
    Token::String(ref s, len) => format!("STR({},{})", s, len),
    Token::Break(_) => "BREAK".to_string(),
    Token::Begin(_) => "BEGIN".to_string(),
    Token::End => "END".to_string(),
    Token::Eof => "EOF".to_string()
  }
}

pub fn buf_str(toks: &[Token],
               szs: &[isize],
               left: usize,
               right: usize,
               lim: usize)
               -> String {
  let n = toks.len();
  assert_eq!(n, szs.len());
  let mut i = left;
  let mut l = lim;
  let mut s = string::String::from("[");
  while i != right && l != 0 {
    l -= 1;
    if i != left {
      s.push_str(", ");
    }
    s.push_str(&format!("{}={}",
                        szs[i],
                        tok_str(&toks[i])));
    i += 1;
    i %= n;
  }
  s.push(']');
  s
}

#[derive(Copy, Clone)]
pub enum PrintStackBreak {
  Fits,
  Broken(Breaks),
}

#[derive(Copy, Clone)]
pub struct PrintStackElem {
  offset: isize,
  pbreak: PrintStackBreak
}

const SIZE_INFINITY: isize = 0xffff;

//-------------------------------------
//   Keywords
//-------------------------------------
pub static VAR: &'static str = "var";

pub fn mk_printer<'a>(out: Box<io::Write+'a>, linewidth: usize) -> Printer<'a> {
  // Yes 3, it makes the ring buffers big enough to never
  // fall behind.
  let n: usize = 3 * linewidth;
  debug!("mk_printer {}", linewidth);
  let token = vec![Token::Eof; n];
  let size = vec![0_isize; n];
  let scan_stack = vec![0_usize; n];
  Printer {
    out: out,
    buf_len: n,
    margin: linewidth as isize,
    space: linewidth as isize,
    left: 0,
    right: 0,
    token: token,
    size: size,
    left_total: 0,
    right_total: 0,
    scan_stack: scan_stack,
    scan_stack_empty: true,
    top: 0,
    bottom: 0,
    print_stack: Vec::new(),
    pending_indentation: 0
  }
}


/// In case you do not have the paper, here is an explanation of what's going
/// on.
///
/// There is a stream of input tokens flowing through this printer.
///
/// The printer buffers up to 3N tokens inside itself, where N is linewidth.
/// Yes, linewidth is chars and tokens are multi-char, but in the worst
/// case every token worth buffering is 1 char long, so it's ok.
///
/// Tokens are String, Break, and Begin/End to delimit blocks.
///
/// Begin tokens can carry an offset, saying "how far to indent when you break
/// inside here", as well as a flag indicating "consistent" or "inconsistent"
/// breaking. Consistent breaking means that after the first break, no attempt
/// will be made to flow subsequent breaks together onto lines. Inconsistent
/// is the opposite. Inconsistent breaking example would be, say:
///
///  foo(hello, there, good, friends)
///
/// breaking inconsistently to become
///
///  foo(hello, there
///      good, friends);
///
/// whereas a consistent breaking would yield:
///
///  foo(hello,
///      there
///      good,
///      friends);
///
/// That is, in the consistent-break blocks we value vertical alignment
/// more than the ability to cram stuff onto a line. But in all cases if it
/// can make a block a one-liner, it'll do so.
///
/// Carrying on with high-level logic:
///
/// The buffered tokens go through a ring-buffer, 'tokens'. The 'left' and
/// 'right' indices denote the active portion of the ring buffer as well as
/// describing hypothetical points-in-the-infinite-stream at most 3N tokens
/// apart (i.e. "not wrapped to ring-buffer boundaries"). The paper will switch
/// between using 'left' and 'right' terms to denote the wrapped-to-ring-buffer
/// and point-in-infinite-stream senses freely.
///
/// There is a parallel ring buffer, 'size', that holds the calculated size of
/// each token. Why calculated? Because for Begin/End pairs, the "size"
/// includes everything between the pair. That is, the "size" of Begin is
/// actually the sum of the sizes of everything between Begin and the paired
/// End that follows. Since that is arbitrarily far in the future, 'size' is
/// being rewritten regularly while the printer runs; in fact most of the
/// machinery is here to work out 'size' entries on the fly (and give up when
/// they're so obviously over-long that "infinity" is a good enough
/// approximation for purposes of line breaking).
///
/// The "input side" of the printer is managed as an abstract process called
/// SCAN, which uses 'scan_stack', 'scan_stack_empty', 'top' and 'bottom', to
/// manage calculating 'size'. SCAN is, in other words, the process of
/// calculating 'size' entries.
///
/// The "output side" of the printer is managed by an abstract process called
/// PRINT, which uses 'print_stack', 'margin' and 'space' to figure out what to
/// do with each token/size pair it consumes as it goes. It's trying to consume
/// the entire buffered window, but can't output anything until the size is >=
/// 0 (sizes are set to negative while they're pending calculation).
///
/// So SCAN takes input and buffers tokens and pending calculations, while
/// PRINT gobbles up completed calculations and tokens from the buffer. The
/// theory is that the two can never get more than 3N tokens apart, because
/// once there's "obviously" too much data to fit on a line, in a size
/// calculation, SCAN will write "infinity" to the size and let PRINT consume
/// it.
///
/// In this implementation (following the paper, again) the SCAN process is
/// the method called 'pretty_print', and the 'PRINT' process is the method
/// called 'print'.
pub struct Printer<'a> {
  pub out: Box<io::Write+'a>,
  buf_len: usize,
  /// Width of lines we're constrained to
  margin: isize,
  /// Number of spaces left on line
  space: isize,
  /// Index of left side of input stream
  left: usize,
  /// Index of right side of input stream
  right: usize,
  /// Ring-buffer stream goes through
  token: Vec<Token> ,
  /// Ring-buffer of calculated sizes
  size: Vec<isize> ,
  /// Running size of stream "...left"
  left_total: isize,
  /// Running size of stream "...right"
  right_total: isize,
  /// Pseudo-stack, really a ring too. Holds the
  /// primary-ring-buffers index of the Begin that started the
  /// current block, possibly with the most recent Break after that
  /// Begin (if there is any) on top of it. Stuff is flushed off the
  /// bottom as it becomes irrelevant due to the primary ring-buffer
  /// advancing.
  scan_stack: Vec<usize> ,
  /// Top==bottom disambiguator
  scan_stack_empty: bool,
  /// Index of top of scan_stack
  top: usize,
  /// Index of bottom of scan_stack
  bottom: usize,
  /// Stack of blocks-in-progress being flushed by print
  print_stack: Vec<PrintStackElem> ,
  /// Buffered indentation to avoid writing trailing whitespace
  pending_indentation: isize,
}

impl<'a> Printer<'a> {
  pub fn last_token(&mut self) -> Token {
    self.token[self.right].clone()
  }
  // be very careful with this!
  pub fn replace_last_token(&mut self, t: Token) {
    self.token[self.right] = t;
  }
  pub fn pretty_print(&mut self, token: Token) -> io::Result<()> {
    debug!("pp Vec<{},{}>", self.left, self.right);
    match token {
      Token::Eof => {
        if !self.scan_stack_empty {
          self.check_stack(0);
          try!(self.advance_left());
        }
        self.indent(0);
        Ok(())
      }
      Token::Begin(b) => {
        if self.scan_stack_empty {
          self.left_total = 1;
          self.right_total = 1;
          self.left = 0;
          self.right = 0;
        } else { self.advance_right(); }
        debug!("pp Begin({})/buffer Vec<{},{}>",
               b.offset, self.left, self.right);
        self.token[self.right] = token;
        self.size[self.right] = -self.right_total;
        let right = self.right;
        self.scan_push(right);
        Ok(())
      }
      Token::End => {
        if self.scan_stack_empty {
          debug!("pp End/print Vec<{},{}>", self.left, self.right);
          self.print(token, 0)
        } else {
          debug!("pp End/buffer Vec<{},{}>", self.left, self.right);
          self.advance_right();
          self.token[self.right] = token;
          self.size[self.right] = -1;
          let right = self.right;
          self.scan_push(right);
          Ok(())
        }
      }
      Token::Break(b) => {
        if self.scan_stack_empty {
          self.left_total = 1;
          self.right_total = 1;
          self.left = 0;
          self.right = 0;
        } else { self.advance_right(); }
        debug!("pp Break({})/buffer Vec<{},{}>",
               b.offset, self.left, self.right);
        self.check_stack(0);
        let right = self.right;
        self.scan_push(right);
        self.token[self.right] = token;
        self.size[self.right] = -self.right_total;
        self.right_total += b.blank_space;
        Ok(())
      }
      Token::String(s, len) => {
        if self.scan_stack_empty {
          debug!("pp String('{}')/print Vec<{},{}>",
                 s, self.left, self.right);
          self.print(Token::String(s, len), len)
        } else {
          debug!("pp String('{}')/buffer Vec<{},{}>",
                 s, self.left, self.right);
          self.advance_right();
          self.token[self.right] = Token::String(s, len);
          self.size[self.right] = len;
          self.right_total += len;
          self.check_stream()
        }
      }
    }
  }
  pub fn check_stream(&mut self) -> io::Result<()> {
    debug!("check_stream Vec<{}, {}> with left_total={}, right_total={}",
           self.left, self.right, self.left_total, self.right_total);
    if self.right_total - self.left_total > self.space {
      debug!("scan window is {}, longer than space on line ({})",
             self.right_total - self.left_total, self.space);
      if !self.scan_stack_empty {
        if self.left == self.scan_stack[self.bottom] {
          debug!("setting {} to infinity and popping", self.left);
          let scanned = self.scan_pop_bottom();
          self.size[scanned] = SIZE_INFINITY;
        }
      }
      try!(self.advance_left());
      if self.left != self.right {
        try!(self.check_stream());
      }
    }
    Ok(())
  }
  pub fn scan_push(&mut self, x: usize) {
    debug!("scan_push {}", x);
    if self.scan_stack_empty {
      self.scan_stack_empty = false;
    } else {
      self.top += 1;
      self.top %= self.buf_len;
      assert!((self.top != self.bottom));
    }
    self.scan_stack[self.top] = x;
  }
  pub fn scan_pop(&mut self) -> usize {
    assert!((!self.scan_stack_empty));
    let x = self.scan_stack[self.top];
    if self.top == self.bottom {
      self.scan_stack_empty = true;
    } else {
      self.top += self.buf_len - 1; self.top %= self.buf_len;
    }
    return x;
  }
  pub fn scan_top(&mut self) -> usize {
    assert!((!self.scan_stack_empty));
    return self.scan_stack[self.top];
  }
  pub fn scan_pop_bottom(&mut self) -> usize {
    assert!((!self.scan_stack_empty));
    let x = self.scan_stack[self.bottom];
    if self.top == self.bottom {
      self.scan_stack_empty = true;
    } else {
      self.bottom += 1; self.bottom %= self.buf_len;
    }
    return x;
  }
  pub fn advance_right(&mut self) {
    self.right += 1;
    self.right %= self.buf_len;
    assert!((self.right != self.left));
  }
  pub fn advance_left(&mut self) -> io::Result<()> {
    debug!("advance_left Vec<{},{}>, sizeof({})={}", self.left, self.right,
           self.left, self.size[self.left]);

    let mut left_size = self.size[self.left];

    while left_size >= 0 {
      let left = self.token[self.left].clone();

      let len = match left {
        Token::Break(b) => b.blank_space,
        Token::String(_, len) => {
          assert_eq!(len, left_size);
          len
        }
        _ => 0
      };

      try!(self.print(left, left_size));

      self.left_total += len;

      if self.left == self.right {
        break;
      }

      self.left += 1;
      self.left %= self.buf_len;

      left_size = self.size[self.left];
    }

    Ok(())
  }
  pub fn check_stack(&mut self, k: isize) {
    if !self.scan_stack_empty {
      let x = self.scan_top();
      match self.token[x] {
        Token::Begin(_) => {
          if k > 0 {
            let popped = self.scan_pop();
            self.size[popped] = self.size[x] + self.right_total;
            self.check_stack(k - 1);
          }
        }
        Token::End => {
          // paper says + not =, but that makes no sense.
          let popped = self.scan_pop();
          self.size[popped] = 1;
          self.check_stack(k + 1);
        }
        _ => {
          let popped = self.scan_pop();
          self.size[popped] = self.size[x] + self.right_total;
          if k > 0 {
            self.check_stack(k);
          }
        }
      }
    }
  }
  pub fn print_newline(&mut self, amount: isize) -> io::Result<()> {
    debug!("NEWLINE {}", amount);
    let ret = write!(self.out, "\n");
    self.pending_indentation = 0;
    self.indent(amount);
    return ret;
  }
  pub fn indent(&mut self, amount: isize) {
    debug!("INDENT {}", amount);
    self.pending_indentation += amount;
  }
  pub fn get_top(&mut self) -> PrintStackElem {
    let print_stack = &mut self.print_stack;
    let n = print_stack.len();
    if n != 0 {
      (*print_stack)[n - 1]
    } else {
      PrintStackElem {
        offset: 0,
        pbreak: PrintStackBreak::Broken(Breaks::Inconsistent)
      }
    }
  }
  pub fn print_str(&mut self, s: &str) -> io::Result<()> {
    while self.pending_indentation > 0 {
      try!(write!(self.out, " "));
      self.pending_indentation -= 1;
    }
    write!(self.out, "{}", s)
  }
  pub fn print(&mut self, token: Token, l: isize) -> io::Result<()> {
    debug!("print {} {} (remaining line space={})", tok_str(&token), l,
           self.space);
    debug!("{}", buf_str(&self.token,
                         &self.size,
                         self.left,
                         self.right,
                         6));
    match token {
      Token::Begin(b) => {
        if l > self.space {
          let col = self.margin - self.space + b.offset;
          debug!("print Begin -> push broken block at col {}", col);
          self.print_stack.push(PrintStackElem {
            offset: col,
            pbreak: PrintStackBreak::Broken(b.breaks)
          });
        } else {
          debug!("print Begin -> push fitting block");
          self.print_stack.push(PrintStackElem {
            offset: 0,
            pbreak: PrintStackBreak::Fits
          });
        }
        Ok(())
      }
      Token::End => {
        debug!("print End -> pop End");
        let print_stack = &mut self.print_stack;
        assert!((!print_stack.is_empty()));
        print_stack.pop().unwrap();
        Ok(())
      }
      Token::Break(b) => {
        let top = self.get_top();
        match top.pbreak {
          PrintStackBreak::Fits => {
            debug!("print Break({}) in fitting block", b.blank_space);
            self.space -= b.blank_space;
            self.indent(b.blank_space);
            Ok(())
          }
          PrintStackBreak::Broken(Breaks::Consistent) => {
            debug!("print Break({}+{}) in consistent block",
                   top.offset, b.offset);
            let ret = self.print_newline(top.offset + b.offset);
            self.space = self.margin - (top.offset + b.offset);
            ret
          }
          PrintStackBreak::Broken(Breaks::Inconsistent) => {
            if l > self.space {
              debug!("print Break({}+{}) w/ newline in inconsistent",
                     top.offset, b.offset);
              let ret = self.print_newline(top.offset + b.offset);
              self.space = self.margin - (top.offset + b.offset);
              ret
            } else {
              debug!("print Break({}) w/o newline in inconsistent",
                     b.blank_space);
              self.indent(b.blank_space);
              self.space -= b.blank_space;
              Ok(())
            }
          }
        }
      }
      Token::String(s, len) => {
        debug!("print String({})", s);
        assert_eq!(l, len);
        // assert!(l <= space);
        self.space -= len;
        self.print_str(&s[..])
      }
      Token::Eof => {
        // Eof should never get here.
        panic!();
      }
    }
  }
}

// Convenience functions to talk to the printer.
//
// "raw box"
pub fn rbox(p: &mut Printer, indent: usize, b: Breaks) -> io::Result<()> {
  p.pretty_print(Token::Begin(BeginToken {
    offset: indent as isize,
    breaks: b
  }))
}

pub fn ibox(p: &mut Printer, indent: usize) -> io::Result<()> {
  rbox(p, indent, Breaks::Inconsistent)
}

pub fn cbox(p: &mut Printer, indent: usize) -> io::Result<()> {
  rbox(p, indent, Breaks::Consistent)
}

pub fn break_offset(p: &mut Printer, n: usize, off: isize) -> io::Result<()> {
  p.pretty_print(Token::Break(BreakToken {
    offset: off,
    blank_space: n as isize
  }))
}

pub fn end(p: &mut Printer) -> io::Result<()> {
  p.pretty_print(Token::End)
}

pub fn eof(p: &mut Printer) -> io::Result<()> {
  p.pretty_print(Token::Eof)
}

pub fn word(p: &mut Printer, wrd: &str) -> io::Result<()> {
  p.pretty_print(Token::String(/* bad */ wrd.to_string(), wrd.len() as isize))
}

pub fn huge_word(p: &mut Printer, wrd: &str) -> io::Result<()> {
  p.pretty_print(Token::String(/* bad */ wrd.to_string(), SIZE_INFINITY))
}

pub fn zero_word(p: &mut Printer, wrd: &str) -> io::Result<()> {
  p.pretty_print(Token::String(/* bad */ wrd.to_string(), 0))
}

pub fn spaces(p: &mut Printer, n: usize) -> io::Result<()> {
  break_offset(p, n, 0)
}

pub fn zerobreak(p: &mut Printer) -> io::Result<()> {
  spaces(p, 0)
}

pub fn space(p: &mut Printer) -> io::Result<()> {
  spaces(p, 1)
}

pub fn hardbreak(p: &mut Printer) -> io::Result<()> {
  spaces(p, SIZE_INFINITY as usize)
}

pub fn hardbreak_tok_offset(off: isize) -> Token {
  Token::Break(BreakToken {offset: off, blank_space: SIZE_INFINITY})
}

pub fn hardbreak_tok() -> Token {
  hardbreak_tok_offset(0)
}

pub enum AnnNode<'a> {
  NodeIdent(&'a ast::Ident),
  NodeName(&'a ast::Name),
  NodeBlock(&'a ast::Block),
  NodeItem(&'a ast::Item),
  NodeSubItem(ast::NodeId),
  NodeExpr(&'a ast::Expr),
  NodePat(&'a ast::Pat),
}

pub trait PpAnn {
  fn pre(&self, _state: &mut State, _node: AnnNode) -> io::Result<()> { Ok(()) }
  fn post(&self, _state: &mut State, _node: AnnNode) -> io::Result<()> { Ok(()) }
}

#[derive(Copy, Clone)]
pub struct NoAnn;

impl PpAnn for NoAnn {}

#[derive(Copy, Clone)]
pub struct CurrentCommentAndLiteral {
  pub cur_cmnt: usize,
  pub cur_lit: usize,
}

pub struct State<'a> {
  pub s: Printer<'a>,
  cm: Option<&'a CodeMap>,
  comments: Option<Vec<comments::Comment> >,
  literals: Option<Vec<comments::Literal> >,
  cur_cmnt_and_lit: CurrentCommentAndLiteral,
  boxes: Vec<Breaks>,
  ann: &'a (PpAnn+'a),
}

pub fn rust_printer<'a>(writer: Box<Write+'a>) -> State<'a> {
  static NO_ANN: NoAnn = NoAnn;
  rust_printer_annotated(writer, &NO_ANN)
}

pub fn rust_printer_annotated<'a>(writer: Box<Write+'a>,
                                  ann: &'a PpAnn) -> State<'a> {
  State {
    s: mk_printer(writer, DEFAULT_COLUMNS),
    cm: None,
    comments: None,
    literals: None,
    cur_cmnt_and_lit: CurrentCommentAndLiteral {
      cur_cmnt: 0,
      cur_lit: 0
    },
    boxes: Vec::new(),
    ann: ann,
  }
}

pub const INDENT_UNIT: usize = 4;

pub const DEFAULT_COLUMNS: usize = 78;

/// Requires you to pass an input filename and reader so that
/// it can scan the input text for comments and literals to
/// copy forward.
pub fn print_package<'a>(cm: &'a CodeMap,
                       span_diagnostic: &errors::Handler,
                       pkg: &ast::Package,
                       filename: String,
                       input: &mut Read,
                       out: Box<Write+'a>,
                       ann: &'a PpAnn,
                       is_expanded: bool) -> io::Result<()> {
  let mut s = State::new_from_input(cm,
                                    span_diagnostic,
                                    filename,
                                    input,
                                    out,
                                    ann,
                                    is_expanded);
  try!(s.print_mod(&pkg.module, &pkg.attrs));
  //try!(s.print_remaining_comments());
  eof(&mut s.s)
}

pub trait PrintState<'a> {
  fn writer(&mut self) -> &mut Printer<'a>;
  fn boxes(&mut self) -> &mut Vec<Breaks>;
  fn comments(&mut self) -> &mut Option<Vec<comments::Comment>>;
  fn cur_cmnt_and_lit(&mut self) -> &mut CurrentCommentAndLiteral;
  fn literals(&self) -> &Option<Vec<comments::Literal>>;

  fn word_space(&mut self, w: &str) -> io::Result<()> {
    try!(word(self.writer(), w));
    space(self.writer())
  }

  fn popen(&mut self) -> io::Result<()> { word(self.writer(), "(") }

  fn pclose(&mut self) -> io::Result<()> { word(self.writer(), ")") }
}

impl<'a> PrintState<'a> for State<'a> {
  fn writer(&mut self) -> &mut Printer<'a> {
    &mut self.s
  }

  fn boxes(&mut self) -> &mut Vec<Breaks> {
    &mut self.boxes
  }

  fn comments(&mut self) -> &mut Option<Vec<comments::Comment>> {
    &mut self.comments
  }

  fn cur_cmnt_and_lit(&mut self) -> &mut CurrentCommentAndLiteral {
    &mut self.cur_cmnt_and_lit
  }

  fn literals(&self) -> &Option<Vec<comments::Literal>> {
    &self.literals
  }
}

impl<'a> State<'a> {
  pub fn new_from_input(cm: &'a CodeMap,
                        span_diagnostic: &errors::Handler,
                        filename: String,
                        input: &mut Read,
                        out: Box<Write + 'a>,
                        ann: &'a PpAnn,
                        is_expanded: bool) -> State<'a> {
    let (cmnts, lits) = comments::gather_comments_and_literals(
      span_diagnostic,
      filename,
      input);

    State::new(
      cm,
      out,
      ann,
      Some(cmnts),
      // If the code is post expansion, don't use the table of
      // literals, since it doesn't correspond with the literals
      // in the AST anymore.
      if is_expanded { None } else { Some(lits) })
  }

  pub fn new(cm: &'a CodeMap,
             out: Box<Write + 'a>,
             ann: &'a PpAnn,
             comments: Option<Vec<comments::Comment>>,
             literals: Option<Vec<comments::Literal>>) -> State<'a> {
    State {
      s: mk_printer(out, DEFAULT_COLUMNS),
      cm: Some(cm),
      comments: comments,
      literals: literals,
      cur_cmnt_and_lit: CurrentCommentAndLiteral {
        cur_cmnt: 0,
        cur_lit: 0
      },
      boxes: Vec::new(),
      ann: ann,
    }
  }

  pub fn cbox(&mut self, u: usize) -> io::Result<()> {
    self.boxes.push(Breaks::Consistent);
    cbox(&mut self.s, u)
  }

  // "raw box"
  fn rbox(&mut self, u: usize, b: Breaks) -> io::Result<()> {
    self.boxes().push(b);
    rbox(self.writer(), u, b)
  }

  fn ibox(&mut self, u: usize) -> io::Result<()> {
    self.boxes().push(Breaks::Inconsistent);
    ibox(self.writer(), u)
  }

  pub fn word_nbsp(&mut self, w: &str) -> io::Result<()> {
    word(&mut self.s, w)?;
    self.nbsp()
  }

  fn nbsp(&mut self) -> io::Result<()> { word(self.writer(), " ") }

  fn space_if_not_bol(&mut self) -> io::Result<()> {
    if !self.is_bol() { try!(space(self.writer())); }
    Ok(())
  }

  pub fn head(&mut self, w: &str) -> io::Result<()> {
    // outer-box is consistent
    self.cbox(INDENT_UNIT)?;
    // head-box is inconsistent
    self.ibox(w.len() + 1)?;
    // keyword that starts the head
    if !w.is_empty() {
      self.word_nbsp(w)?;
    }
    Ok(())
  }

  fn end(&mut self) -> io::Result<()> {
    self.boxes().pop().unwrap();
    end(self.writer())
  }

  fn commasep<T, F>(&mut self, b: Breaks, elts: &[T], mut op: F) -> io::Result<()>
    where F: FnMut(&mut Self, &T) -> io::Result<()>,
  {
    try!(self.rbox(0, b));
    let mut first = true;
    for elt in elts {
      if first { first = false; } else { try!(self.word_space(",")); }
      try!(op(self, elt));
    }
    self.end()
  }

  fn is_begin(&mut self) -> bool {
    match self.writer().last_token() {
      Token::Begin(_) => true,
      _ => false,
    }
  }

  fn is_end(&mut self) -> bool {
    match self.writer().last_token() {
      Token::End => true,
      _ => false,
    }
  }

  // is this the beginning of a line?
  fn is_bol(&mut self) -> bool {
    self.writer().last_token().is_eof() || self.writer().last_token().is_hardbreak_tok()
  }

  fn hardbreak_if_not_bol(&mut self) -> io::Result<()> {
    if !self.is_bol() {
      hardbreak(self.writer())?
    }
    Ok(())
  }

  pub fn break_offset_if_not_bol(&mut self, n: usize,
                                 off: isize) -> io::Result<()> {
    if !self.is_bol() {
      break_offset(&mut self.s, n, off)
    } else {
      if off != 0 && self.s.last_token().is_hardbreak_tok() {
        // We do something pretty sketchy here: tuck the nonzero
        // offset-adjustment we were going to deposit along with the
        // break into the previous hardbreak.
        self.s.replace_last_token(hardbreak_tok_offset(off));
      }
      Ok(())
    }
  }

  pub fn bopen(&mut self) -> io::Result<()> {
    try!(word(&mut self.s, "{"));
    self.end() // close the head-box
  }

  pub fn bclose_(&mut self, span: codemap::Span,
                 indented: usize) -> io::Result<()> {
    self.bclose_maybe_open(span, indented, true)
  }

  pub fn bclose(&mut self, span: codemap::Span) -> io::Result<()> {
    self.bclose_(span, INDENT_UNIT)
  }

  pub fn bclose_maybe_open(&mut self, span: codemap::Span,
                           indented: usize, close_box: bool) -> io::Result<()> {
    self.maybe_print_comment(span.hi)?;
    self.break_offset_if_not_bol(1, -(indented as isize))?;
    word(&mut self.s, "}")?;
    if close_box {
      self.end()?; // close the outer-box
    }
    Ok(())
  }

  // Synthesizes a comment that was not textually present in the original source
  // file.
  pub fn synth_comment(&mut self, text: String) -> io::Result<()> {
    word(&mut self.s, "/*")?;
    space(&mut self.s)?;
    word(&mut self.s, &text[..])?;
    space(&mut self.s)?;
    word(&mut self.s, "*/")
  }

  pub fn commasep_cmnt<T, F, G>(&mut self,
                                b: Breaks,
                                elts: &[T],
                                mut op: F,
                                mut get_span: G) -> io::Result<()> where
                                    F: FnMut(&mut State, &T) -> io::Result<()>,
                                    G: FnMut(&T) -> codemap::Span,
  {
    self.rbox(0, b)?;
    let len = elts.len();
    let mut i = 0;
    for elt in elts {
      self.maybe_print_comment(get_span(elt).hi)?;
      op(self, elt)?;
      i += 1;
      if i < len {
        word(&mut self.s, ",")?;
        self.maybe_print_trailing_comment(get_span(elt),
                                          Some(get_span(&elts[i]).hi))?;
        self.space_if_not_bol()?;
      }
    }
    self.end()
  }

  pub fn commasep_exprs(&mut self, b: Breaks,
                        exprs: &[P<ast::Expr>]) -> io::Result<()> {
    self.commasep_cmnt(b, exprs, |s, e| s.print_expr(&e), |e| e.span)
  }

  fn maybe_print_comment(&mut self, pos: BytePos) -> io::Result<()> {
    loop {
      match self.next_comment() {
        Some(ref cmnt) => {
          if (*cmnt).pos < pos {
            try!(self.print_comment(cmnt));
            self.cur_cmnt_and_lit().cur_cmnt += 1;
          } else { break; }
        }
        _ => break
      }
    }
    Ok(())
  }

  fn next_comment(&mut self) -> Option<comments::Comment> {
    let cur_cmnt = self.cur_cmnt_and_lit().cur_cmnt;
    match *self.comments() {
      Some(ref cmnts) => {
        if cur_cmnt < cmnts.len() {
          Some(cmnts[cur_cmnt].clone())
        } else {
          None
        }
      }
      _ => None
    }
  }

  pub fn maybe_print_trailing_comment(&mut self, span: codemap::Span,
                                      next_pos: Option<BytePos>)
                                      -> io::Result<()> {
    let cm = match self.cm {
      Some(cm) => cm,
      _ => return Ok(())
    };
    match self.next_comment() {
      Some(ref cmnt) => {
        if (*cmnt).style != comments::Trailing { return Ok(()) }
        let span_line = cm.lookup_char_pos(span.hi);
        let comment_line = cm.lookup_char_pos((*cmnt).pos);
        let mut next = (*cmnt).pos + BytePos(1);
        match next_pos { None => (),
          Some(p) => next = p }
        if span.hi < (*cmnt).pos && (*cmnt).pos < next &&
            span_line.line == comment_line.line {
          try!(self.print_comment(cmnt));
          self.cur_cmnt_and_lit.cur_cmnt += 1;
        }
      }
      _ => ()
    }
    Ok(())
  }


  fn print_comment(&mut self,
                   cmnt: &comments::Comment) -> io::Result<()> {
    match cmnt.style {
      comments::Mixed => {
        assert_eq!(cmnt.lines.len(), 1);
        try!(zerobreak(self.writer()));
        try!(word(self.writer(), &cmnt.lines[0]));
        zerobreak(self.writer())
      }
      comments::Isolated => {
        try!(self.hardbreak_if_not_bol());
        for line in &cmnt.lines {
          // Don't print empty lines because they will end up as trailing
          // whitespace
          if !line.is_empty() {
            try!(word(self.writer(), &line[..]));
          }
          try!(hardbreak(self.writer()));
        }
        Ok(())
      }
      comments::Trailing => {
        try!(word(self.writer(), " "));
        if cmnt.lines.len() == 1 {
          try!(word(self.writer(), &cmnt.lines[0]));
          hardbreak(self.writer())
        } else {
          try!(self.ibox(0));
          for line in &cmnt.lines {
            if !line.is_empty() {
              try!(word(self.writer(), &line[..]));
            }
            try!(hardbreak(self.writer()));
          }
          self.end()
        }
      }
      comments::BlankLine => {
        // We need to do at least one, possibly two hardbreaks.
        let is_semi = match self.writer().last_token() {
          Token::String(s, _) => ";" == s,
          _ => false
        };
        if is_semi || self.is_begin() || self.is_end() {
          try!(hardbreak(self.writer()));
        }
        hardbreak(self.writer())
      }
    }
  }

  pub fn print_ident(&mut self, ident: ast::Ident) -> io::Result<()> {
    word(&mut self.s, &ident.name.as_str())?;
    self.ann.post(self, NodeIdent(&ident))
  }

  pub fn print_usize(&mut self, i: usize) -> io::Result<()> {
    word(&mut self.s, &i.to_string())
  }

  pub fn print_expr_maybe_paren(&mut self, expr: &ast::Expr) -> io::Result<()> {
    let needs_par = needs_parentheses(expr);
    if needs_par {
      self.popen()?;
    }
    self.print_expr(expr)?;
    if needs_par {
      self.pclose()?;
    }
    Ok(())
  }

  pub fn print_mod(&mut self, _mod: &ast::Module,
                   attrs: &[ast::Attribute]) -> io::Result<()> {
    for item in &_mod.items {
      try!(self.print_item(&item));
    }
    Ok(())
  }

  /// Pretty-print an item
  pub fn print_item(&mut self, item: &ast::Item) -> io::Result<()> {
    self.hardbreak_if_not_bol()?;
    self.maybe_print_comment(item.span.lo)?;
    self.ann.pre(self, NodeItem(item))?;

    match item.node {
      ast::ItemKind::Import(ref vp) => {
        self.head(&visibility_qualified(&item.vis, "import"))?;
        self.print_view_path(&vp)?;
        word(&mut self.s, ";")?;
        self.end()?; // end inner head-block
        self.end()?; // end outer head-block
      }
      ast::ItemKind::ForeignMod(ref nmod) => {
        self.head("extern")?;
        self.word_nbsp(&nmod.abi.to_string())?;
        self.bopen()?;
        self.print_foreign_mod(nmod, &item.attrs)?;
        self.bclose(item.span)?;
      }
      ast::ItemKind::Static(ref ty, m, ref expr) => {
        self.head(&visibility_qualified(&item.vis,
                                        "static"))?;
        if m == ast::Mutability::Mutable {
          self.word_space(VAR)?;
        }
        self.print_ident(item.ident)?;
        self.word_space(":")?;
        self.print_type(&ty)?;
        space(&mut self.s)?;
        self.end()?; // end the head-ibox

        self.word_space("=")?;
        self.print_expr(&expr)?;
        word(&mut self.s, ";")?;
        self.end()?; // end the outer cbox
      }
      ast::ItemKind::Const(ref ty, ref expr) => {
        self.head(&visibility_qualified(&item.vis,
                                        "const"))?;
        self.print_ident(item.ident)?;
        self.word_space(":")?;
        self.print_type(&ty)?;
        space(&mut self.s)?;
        self.end()?; // end the head-ibox

        self.word_space("=")?;
        self.print_expr(&expr)?;
        word(&mut self.s, ";")?;
        self.end()?; // end the outer cbox
      }
      ast::ItemKind::Fn(ref decl, unsafety, constness, abi, ref body) => {
        self.head("")?;
        self.print_fn(
          decl,
          unsafety,
          constness,
          abi,
          Some(item.ident),
          &item.vis
        )?;
        word(&mut self.s, " ")?;
        self.print_block_with_attrs(&body, &item.attrs)?;
      }
      ast::ItemKind::Ty(ref ty) => {
        self.ibox(INDENT_UNIT)?;
        self.ibox(0)?;
        self.word_nbsp(&visibility_qualified(&item.vis, "type"))?;
        self.print_ident(item.ident)?;
        self.end()?; // end the inner ibox

        space(&mut self.s)?;
        self.word_space("=")?;
        self.print_type(&ty)?;
        word(&mut self.s, ";")?;
        self.end()?; // end the outer ibox
      }
      _ => {
        unimplemented!()
      }
    }
    self.ann.post(self, NodeItem(item))
  }

  pub fn print_view_path(&mut self, vp: &ast::ViewPath) -> io::Result<()> {
    match vp.node {
      ast::ViewPathSimple(ident, ref path) => {
        self.print_path(path, false, 0)?;

        if path.segments.last().unwrap().identifier.name != ident.name {
          space(&mut self.s)?;
          self.word_space("as")?;
          self.print_ident(ident)?;
        }

        Ok(())
      }
      ast::ViewPathGlob(ref path) => {
        self.print_path(path, false, 0)?;
        word(&mut self.s, "::*")
      }
      ast::ViewPathList(ref path, ref idents) => {
        if path.segments.is_empty() {
          word(&mut self.s, "{")?;
        } else {
          self.print_path(path, false, 0)?;
          word(&mut self.s, "::{")?;
        }
        self.commasep(Inconsistent, &idents[..], |s, w| {
          match w.node {
            ast::PathListItemKind::Ident { name, rename, .. } => {
              s.print_ident(name)?;
              if let Some(ident) = rename {
                space(&mut s.s)?;
                s.word_space("as")?;
                s.print_ident(ident)?;
              }
              Ok(())
            },
            ast::PathListItemKind::Mod { rename, .. } => {
              word(&mut s.s, "self")?;
              if let Some(ident) = rename {
                space(&mut s.s)?;
                s.word_space("as")?;
                s.print_ident(ident)?;
              }
              Ok(())
            }
          }
        })?;
        word(&mut self.s, "}")
      }
    }
  }

  fn print_path(&mut self,
                path: &ast::Path,
                colons_before_params: bool,
                depth: usize)
                -> io::Result<()>
  {
    self.maybe_print_comment(path.span.lo)?;

    let mut first = !path.global;
    for segment in &path.segments[..path.segments.len() - depth] {
      if first {
        first = false
      } else {
        word(&mut self.s, "::")?
      }

      self.print_ident(segment.identifier)?;
    }

    Ok(())
  }

  fn print_qpath(&mut self,
                 path: &ast::Path,
                 qself: &ast::QSelf,
                 colons_before_params: bool)
                 -> io::Result<()> {
    word(&mut self.s, "<")?;
    self.print_type(&qself.ty)?;
    if qself.position > 0 {
      space(&mut self.s)?;
      self.word_space("as")?;
      let depth = path.segments.len() - qself.position;
      self.print_path(&path, false, depth)?;
    }
    word(&mut self.s, ">")?;
    word(&mut self.s, "::")?;
    let item_segment = path.segments.last().unwrap();
    self.print_ident(item_segment.identifier)
  }

  pub fn print_foreign_mod(&mut self, nmod: &ast::ForeignMod,
                           attrs: &[ast::Attribute]) -> io::Result<()> {
    //self.print_inner_attributes(attrs)?;
    for item in &nmod.items {
      self.print_foreign_item(item)?;
    }
    Ok(())
  }

  pub fn print_foreign_item(&mut self,
                            item: &ast::ForeignItem) -> io::Result<()> {
    self.hardbreak_if_not_bol()?;
    self.maybe_print_comment(item.span.lo)?;
    //self.print_outer_attributes(&item.attrs)?;
    match item.node {
      ast::ForeignItemKind::Fn(ref decl) => {
        self.head("")?;
        self.print_fn(decl,
                      ast::Unsafety::Normal,
                      ast::Constness::NotConst,
                      Abi::Rust,
                      Some(item.ident),
                      &item.vis)?;
        self.end()?; // end head-ibox
        word(&mut self.s, ";")?;
        self.end() // end the outer fn box
      }
      ast::ForeignItemKind::Static(ref t, m) => {
        self.head(&visibility_qualified(&item.vis, "static"))?;
        if m {
          self.word_space(VAR)?;
        }
        self.print_ident(item.ident)?;
        self.word_space(":")?;
        self.print_type(&t)?;
        word(&mut self.s, ";")?;
        self.end()?; // end the head-ibox
        self.end() // end the outer cbox
      }
    }
  }

  pub fn print_fn(&mut self,
                  decl: &ast::FnDecl,
                  unsafety: ast::Unsafety,
                  constness: ast::Constness,
                  abi: Abi,
                  name: Option<ast::Ident>,
                  vis: &ast::Visibility) -> io::Result<()> {

    self.print_fn_header_info(unsafety, constness, abi, vis)?;

    if let Some(name) = name {
      self.nbsp()?;
      self.print_ident(name)?;
    }

    self.print_fn_args_and_ret(decl)
  }

  pub fn print_fn_header_info(&mut self,
                              unsafety: ast::Unsafety,
                              constness: ast::Constness,
                              abi: Abi,
                              vis: &ast::Visibility) -> io::Result<()> {
    word(&mut self.s, &visibility_qualified(vis, ""))?;

    match constness {
      ast::Constness::NotConst => {}
      ast::Constness::Const => try!(self.word_nbsp("const"))
    }

    self.print_unsafety(unsafety)?;

    if abi != Abi::Rust {
      self.word_nbsp("extern")?;
      self.word_nbsp(&abi.to_string())?;
    }

    word(&mut self.s, "fn")
  }

  pub fn print_unsafety(&mut self, s: ast::Unsafety) -> io::Result<()> {
    match s {
      ast::Unsafety::Normal => Ok(()),
      ast::Unsafety::Unsafe => self.word_nbsp("unsafe"),
    }
  }

  pub fn print_fn_args_and_ret(&mut self, decl: &ast::FnDecl)
                               -> io::Result<()> {
    self.popen()?;
    self.print_fn_args(decl, false)?;
    if decl.variadic {
      word(&mut self.s, ", ...")?;
    }
    self.pclose()?;

    self.print_fn_output(decl)
  }

  pub fn print_fn_args(&mut self, decl: &ast::FnDecl,
                       is_closure: bool) -> io::Result<()> {
    self.rbox(0, Inconsistent)?;
    let mut first = true;
    for arg in &decl.inputs[..] {
      if first { first = false; } else { self.word_space(",")?; }
      self.print_arg(arg, is_closure)?;
    }
    self.end()
  }

  pub fn print_arg(&mut self, input: &ast::Arg, is_closure: bool) -> io::Result<()> {
    self.ibox(INDENT_UNIT)?;
    match input.ty.node {
      ast::TyKind::Infer if is_closure => self.print_pat(&input.pat)?,
      _ => {
        let invalid = if let PatKind::Ident(_, ident, _) = input.pat.node {
          ident.node.name == keywords::Invalid.name()
        } else {
          false
        };
        if !invalid {
          self.print_pat(&input.pat)?;
          word(&mut self.s, ":")?;
          space(&mut self.s)?;
        }
        self.print_type(&input.ty)?;
      }
    }
    self.end()
  }

  pub fn print_fn_output(&mut self, decl: &ast::FnDecl) -> io::Result<()> {
    if let ast::FunctionRetTy::Default(..) = decl.output {
      return Ok(());
    }

    self.space_if_not_bol()?;
    self.ibox(INDENT_UNIT)?;
    self.word_space("->")?;
    match decl.output {
      ast::FunctionRetTy::None(_) =>
        self.word_nbsp("!")?,
      ast::FunctionRetTy::Default(..) => unreachable!(),
      ast::FunctionRetTy::Ty(ref ty) =>
        self.print_type(&ty)?
    }
    self.end()?;

    match decl.output {
      ast::FunctionRetTy::Ty(ref output) => self.maybe_print_comment(output.span.lo),
      _ => Ok(())
    }
  }

  pub fn print_block_with_attrs(&mut self,
                                blk: &ast::Block,
                                attrs: &[ast::Attribute]) -> io::Result<()> {
    self.print_block_maybe_unclosed(blk, INDENT_UNIT, attrs, true)
  }

  pub fn print_block_maybe_unclosed(&mut self,
                                    blk: &ast::Block,
                                    indented: usize,
                                    attrs: &[ast::Attribute],
                                    close_box: bool) -> io::Result<()> {
    match blk.rules {
      BlockCheckMode::Unsafe(..) => try!(self.word_space("unsafe")),
      BlockCheckMode::Default => ()
    }
    self.maybe_print_comment(blk.span.lo)?;
    self.ann.pre(self, NodeBlock(blk))?;
    self.bopen()?;

    //self.print_inner_attributes(attrs)?;

    for st in &blk.stmts {
      self.print_stmt(st)?;
    }
    match blk.expr {
      Some(ref expr) => {
        self.space_if_not_bol()?;
        self.print_expr_outer_attr_style(&expr, false)?;
        self.maybe_print_trailing_comment(expr.span, Some(blk.span.hi))?;
      }
      _ => ()
    }
    self.bclose_maybe_open(blk.span, indented, close_box)?;
    self.ann.post(self, NodeBlock(blk))
  }

  pub fn print_stmt(&mut self, st: &ast::Stmt) -> io::Result<()> {
    self.maybe_print_comment(st.span.lo)?;
    match st.node {
      ast::StmtKind::Decl(ref decl, _) => {
        try!(self.print_decl(&decl));
      }
      ast::StmtKind::Expr(ref expr, _) => {
        try!(self.space_if_not_bol());
        try!(self.print_expr_outer_attr_style(&expr, false));
      }
      ast::StmtKind::Semi(ref expr, _) => {
        try!(self.space_if_not_bol());
        try!(self.print_expr_outer_attr_style(&expr, false));
        try!(word(&mut self.s, ";"));
      }
    }

    if parser::stmt_ends_with_semi(&st.node) {
      word(&mut self.s, ";")?;
    }
    self.maybe_print_trailing_comment(st.span, None)
  }

  pub fn print_block(&mut self, blk: &ast::Block) -> io::Result<()> {
    self.print_block_with_attrs(blk, &[])
  }

  pub fn print_decl(&mut self, decl: &ast::Decl) -> io::Result<()> {
    try!(self.maybe_print_comment(decl.span.lo));
    match decl.node {
      ast::DeclKind::Local(ref loc) => {

        let decl = match loc.mutbl {
          Mutability::Immutable => "let",
          Mutability::Mutable => "var"
        };

        //try!(self.print_outer_attributes(&loc.attrs));
        try!(self.space_if_not_bol());
        try!(self.ibox(INDENT_UNIT));
        try!(self.word_nbsp(decl));

        try!(self.ibox(INDENT_UNIT));
        try!(self.print_local_decl(&loc));
        try!(self.end());
        if let Some(ref init) = loc.init {
          try!(self.nbsp());
          try!(self.word_space("="));
          try!(self.print_expr(&init));
        }
        self.end()
      }

      ast::DeclKind::Item(ref item) => self.print_item(&item)
    }
  }

  pub fn print_local_decl(&mut self, loc: &ast::Local) -> io::Result<()> {
    try!(self.print_pat(&loc.pat));
    if let Some(ref ty) = loc.ty {
      try!(self.word_space(":"));
      try!(self.print_type(&ty));
    }
    Ok(())
  }

  pub fn print_type(&mut self, ty: &ast::Ty) -> io::Result<()> {
    self.maybe_print_comment(ty.span.lo)?;
    self.ibox(0)?;

    match ty.node {
      ast::TyKind::Vec(ref ty) => {
        word(&mut self.s, "[")?;
        self.print_type(&ty)?;
        word(&mut self.s, "]")?;
      }

      ast::TyKind::Path(ref path) => {
        self.print_path(path, false, 0)?;
      }

      ast::TyKind::Tup(ref elts) => {
        self.popen()?;
        self.commasep(Inconsistent, &elts[..],
                           |s, ty| s.print_type(&ty))?;
        if elts.len() == 1 {
          word(&mut self.s, ",")?;
        }
        self.pclose()?;
      }

      ast::TyKind::Paren(ref typ) => {
        self.popen()?;
        self.print_type(&typ)?;
        self.pclose()?;
      }

      ast::TyKind::Infer => {
        word(&mut self.s, "_")?;
      }
    }

    self.end()
  }

  pub fn print_pat(&mut self, pat: &ast::Pat) -> io::Result<()> {
    self.maybe_print_comment(pat.span.lo)?;
    self.ann.pre(self, NodePat(pat))?;

    match pat.node {
      PatKind::Wild => {
        word(&mut self.s, "_");
      },

      PatKind::Ident(binding_mode, ref path1, ref sub) => {
        match binding_mode {
          ast::BindingMode::ByRef => {
            self.word_nbsp("ref")?;
          }
          ast::BindingMode::ByValue => {}
        }

        self.print_ident(path1.node)?;
        if let Some(ref p) = *sub {
          word(&mut self.s, "@")?;
          self.print_pat(&p)?;
        }
      }

      PatKind::Path(ref path) => {
        self.print_path(path, true, 0)?
      },

      PatKind::QPath(ref qself, ref path) => {
        self.print_qpath(path, qself, false)?
      },

      PatKind::Struct(ref path, ref fields, etc) => {
        self.print_path(path, true, 0)?;
        self.nbsp()?;
        self.word_space("{")?;
        self.commasep_cmnt(
          Consistent, &fields[..],
          |s, f| {
            s.cbox(INDENT_UNIT)?;
            if !f.node.is_shorthand {
              s.print_ident(f.node.ident)?;
              s.word_nbsp(":")?;
            }
            s.print_pat(&f.node.pat)?;
            s.end()
          },
          |f| f.node.pat.span)?;
        if etc {
          if !fields.is_empty() { self.word_space(",")?; }
          word(&mut self.s, "..")?;
        }
        space(&mut self.s)?;
        word(&mut self.s, "}")?;
      }

      PatKind::Tuple(ref elts, ddpos) => {
        self.popen()?;
        if let Some(ddpos) = ddpos {
          self.commasep(Inconsistent, &elts[..ddpos], |s, p| s.print_pat(&p))?;
          if ddpos != 0 {
            self.word_space(",")?;
          }
          word(&mut self.s, "..")?;
          if ddpos != elts.len() {
            word(&mut self.s, ",")?;
            self.commasep(Inconsistent, &elts[ddpos..], |s, p| s.print_pat(&p))?;
          }
        } else {
          self.commasep(Inconsistent, &elts[..], |s, p| s.print_pat(&p))?;
          if elts.len() == 1 {
            word(&mut self.s, ",")?;
          }
        }
        self.pclose()?;
      }

      PatKind::TupleStruct(ref path, ref elts, ddpos) => {
        self.print_path(path, true, 0)?;
        self.popen()?;
        if let Some(ddpos) = ddpos {
          self.commasep(Inconsistent, &elts[..ddpos], |s, p| s.print_pat(&p))?;
          if ddpos != 0 {
            self.word_space(",")?;
          }
          word(&mut self.s, "..")?;
          if ddpos != elts.len() {
            word(&mut self.s, ",")?;
            self.commasep(Inconsistent, &elts[ddpos..], |s, p| s.print_pat(&p))?;
          }
        } else {
          self.commasep(Inconsistent, &elts[..], |s, p| s.print_pat(&p))?;
        }
        self.pclose()?;
      }

      PatKind::Vec(ref before, ref slice, ref after) => {
        word(&mut self.s, "[")?;
        self.commasep(Inconsistent,
                      &before[..],
                      |s, p| s.print_pat(&p))?;
        if let Some(ref p) = *slice {
          if !before.is_empty() { self.word_space(",")?; }
          if p.node != PatKind::Wild {
            self.print_pat(&p)?;
          }
          word(&mut self.s, "..")?;
          if !after.is_empty() { self.word_space(",")?; }
        }
        self.commasep(Inconsistent,
                      &after[..],
                      |s, p| s.print_pat(&p))?;
        word(&mut self.s, "]")?;
      }

      PatKind::Range(ref begin, ref end) => {
        self.print_expr(&begin)?;
        space(&mut self.s)?;
        word(&mut self.s, "...")?;
        self.print_expr(&end)?;
      }

      PatKind::Lit(ref e) => { self.print_expr(&**e)?; },
    }

    self.ann.post(self, NodePat(pat))
  }

  pub fn print_expr(&mut self, expr: &ast::Expr) -> io::Result<()> {
    self.print_expr_outer_attr_style(expr, true)
  }

  fn print_expr_outer_attr_style(&mut self,
                                 expr: &ast::Expr,
                                 is_inline: bool) -> io::Result<()> {
    try!(self.maybe_print_comment(expr.span.lo));

    // temporary solution
    let attrs = &Vec::new();
    /*
    let attrs = expr.attrs.as_attr_slice();
    if is_inline {
      try!(self.print_outer_attributes_inline(attrs));
    } else {
      try!(self.print_outer_attributes(attrs));
    }*/

    try!(self.ibox(INDENT_UNIT));
    try!(self.ann.pre(self, NodeExpr(expr)));

    match expr.node {

      ast::ExprKind::Unary(op, ref expr) => {
        self.print_expr_unary(op, &expr)?;
      }

      ast::ExprKind::Binary(op, ref lhs, ref rhs) => {
        self.print_expr_binary(op, &lhs, &rhs)?;
      }

      ast::ExprKind::Cast(ref expr, ref ty) => {
        if let ast::ExprKind::Cast(..) = expr.node {
          self.print_expr(&expr)?;
        } else {
          self.print_expr_maybe_paren(&expr)?;
        }
        space(&mut self.s)?;
        self.word_space("as")?;
        self.print_type(&ty)?;
      }

      ast::ExprKind::Type(ref expr, ref ty) => {
        self.print_expr(&expr)?;
        self.word_space(":")?;
        self.print_type(&ty)?;
      }

      ast::ExprKind::If(ref test, ref blk, ref elseopt) => {
        self.print_if(&test, &blk, elseopt.as_ref().map(|e| &**e))?;
      }
      ast::ExprKind::IfLet(ref pat, ref expr, ref blk, ref elseopt) => {
        self.print_if_let(&pat, &expr, &blk, elseopt.as_ref().map(|e| &**e))?;
      }

      ast::ExprKind::Closure => {
        unimplemented!()
      }

      ast::ExprKind::While(ref test, ref blk, opt_ident) => {
        if let Some(ident) = opt_ident {
          self.print_ident(ident.node)?;
          try!(self.word_space(":"));
        }
        self.head("while")?;
        self.print_expr(&test)?;
        space(&mut self.s)?;
        self.print_block_with_attrs(&blk, attrs)?;
      }
      ast::ExprKind::WhileLet(ref pat, ref expr, ref blk, opt_ident) => {
        if let Some(ident) = opt_ident {
          self.print_ident(ident.node)?;
          self.word_space(":")?;
        }
        self.head("while let")?;
        self.print_pat(&pat)?;
        space(&mut self.s)?;
        self.word_space("=")?;
        self.print_expr(&expr)?;
        space(&mut self.s)?;
        self.print_block_with_attrs(&blk, attrs)?;
      }
      ast::ExprKind::ForLoop(ref pat, ref iter, ref blk, opt_ident) => {
        if let Some(ident) = opt_ident {
          self.print_ident(ident.node)?;
          self.word_space(":")?;
        }
        self.head("for")?;
        self.print_pat(&pat)?;
        space(&mut self.s)?;
        self.word_space("in")?;
        self.print_expr(&iter)?;
        space(&mut self.s)?;
        self.print_block_with_attrs(&blk, attrs)?;
      }
      ast::ExprKind::Loop(ref blk, opt_ident) => {
        if let Some(ident) = opt_ident {
          self.print_ident(ident.node)?;
          self.word_space(":")?;
        }
        self.head("loop")?;
        space(&mut self.s)?;
        self.print_block_with_attrs(&blk, attrs)?;
      }

      ast::ExprKind::Match => {
        unimplemented!()
      }

      ast::ExprKind::Block(ref blk) => {
        // containing cbox, will be closed by print-block at }
        try!(self.cbox(INDENT_UNIT));
        // head-box, will be closed by print-block after {
        try!(self.ibox(0));
        try!(self.print_block_with_attrs(&blk, attrs));
      }

      ast::ExprKind::InPlace(ref place, ref expr) => {
        unimplemented!()
      }

      ast::ExprKind::Assign(ref lhs, ref rhs) => {
        self.print_expr(&lhs)?;
        space(&mut self.s)?;
        self.word_space("=")?;
        self.print_expr(&rhs)?;
      }

      ast::ExprKind::AssignOp(op, ref lhs, ref rhs) => {
        self.print_expr(&lhs)?;
        space(&mut self.s)?;
        word(&mut self.s, op.node.to_string())?;
        self.word_space("=")?;
        self.print_expr(&rhs)?;
      }

      ast::ExprKind::Paren(ref e) => {
        self.popen()?;
//        try!(self.print_inner_attributes_inline(attrs));
        self.print_expr(&e)?;
        self.pclose()?;
      }

      ast::ExprKind::Path(None, ref path) => {
        self.print_path(path, true, 0)?;
      }

      ast::ExprKind::Path(Some(ref qself), ref path) => {
        self.print_qpath(path, qself, true)?;
      }

      ast::ExprKind::Struct(ref path, ref fields, ref wth) => {
        self.print_expr_struct(path, &fields[..], wth, attrs)?;
      }

      ast::ExprKind::Tup(ref exprs) => {
        self.print_expr_tup(&exprs[..], attrs)?;
      }

      ast::ExprKind::Field(ref expr, id) => {
        self.print_expr(&expr)?;
        word(&mut self.s, ".")?;
        self.print_ident(id.node)?;
      }

      ast::ExprKind::TupField(ref expr, id) => {
        self.print_expr(&expr)?;
        word(&mut self.s, ".")?;
        self.print_usize(id.node)?;
      }

      ast::ExprKind::Index(ref expr, ref index) => {
        self.print_expr(&expr)?;
        word(&mut self.s, "[")?;
        self.print_expr(&index)?;
        word(&mut self.s, "]")?;
      }

      ast::ExprKind::Range(ref start, ref end, limits) => {
        if let &Some(ref e) = start {
          self.print_expr(&e)?;
        }
        if limits == ast::RangeLimits::HalfOpen {
          word(&mut self.s, "..")?;
        } else {
          word(&mut self.s, "...")?;
        }
        if let &Some(ref e) = end {
          self.print_expr(&e)?;
        }
      }

      ast::ExprKind::Vec(ref exprs) => {
        self.print_expr_vec(&exprs[..], attrs)?;
      }

      ast::ExprKind::Repeat(ref element, ref count) => {
        self.print_expr_repeat(&element, &count, attrs)?;
      }

      ast::ExprKind::Call(ref func, ref args) => {
        self.print_expr_call(&func, &args[..])?;
      }

      ast::ExprKind::MethodCall(ident, ref tys, ref args) => {
        self.print_expr_method_call(ident, &tys[..], &args[..])?;
      }

      ast::ExprKind::Lit(ref lit) => {
        self.print_literal(&lit)?;
      }
    }

    self.ann.post(self, NodeExpr(expr))?;
    self.end()
  }

  fn print_expr_unary(&mut self,
                      op: ast::UnOp,
                      expr: &ast::Expr) -> io::Result<()> {
    word(&mut self.s, ast::UnOp::to_string(op))?;
    self.print_expr_maybe_paren(expr)
  }

  fn print_expr_binary(&mut self,
                       op: ast::BinOp,
                       lhs: &ast::Expr,
                       rhs: &ast::Expr) -> io::Result<()> {
    if self.check_expr_bin_needs_paren(lhs, op) {
      self.print_expr_maybe_paren(lhs)?;
    } else {
      self.print_expr(lhs)?;
    }
    space(&mut self.s)?;
    self.word_space(op.node.to_string())?;
    if self.check_expr_bin_needs_paren(rhs, op) {
      self.print_expr_maybe_paren(rhs)
    } else {
      self.print_expr(rhs)
    }
  }

  pub fn check_expr_bin_needs_paren(&mut self, sub_expr: &ast::Expr,
                                    binop: ast::BinOp) -> bool {
    match sub_expr.node {
      ast::ExprKind::Binary(ref sub_op, _, _) => {
        if AssocOp::from_ast_binop(sub_op.node).precedence() <
            AssocOp::from_ast_binop(binop.node).precedence() {
          true
        } else {
          false
        }
      }
      _ => true
    }
  }

  pub fn print_if(&mut self, test: &ast::Expr, blk: &ast::Block,
                  elseopt: Option<&ast::Expr>) -> io::Result<()> {
    self.head("if")?;
    self.print_expr(test)?;
    space(&mut self.s)?;
    self.print_block(blk)?;
    self.print_else(elseopt)
  }

  pub fn print_if_let(&mut self, pat: &ast::Pat, expr: &ast::Expr, blk: &ast::Block,
                      elseopt: Option<&ast::Expr>) -> io::Result<()> {
    try!(self.head("if let"));
    try!(self.print_pat(pat));
    try!(space(&mut self.s));
    try!(self.word_space("="));
    try!(self.print_expr(expr));
    try!(space(&mut self.s));
    try!(self.print_block(blk));
    self.print_else(elseopt)
  }

  fn print_else(&mut self, els: Option<&ast::Expr>) -> io::Result<()> {
    match els {
      Some(_else) => {
        match _else.node {
          // "another else-if"
          ast::ExprKind::If(ref i, ref then, ref e) => {
            try!(self.cbox(INDENT_UNIT - 1));
            try!(self.ibox(0));
            try!(word(&mut self.s, " else if "));
            try!(self.print_expr(&i));
            try!(space(&mut self.s));
            try!(self.print_block(&then));
            self.print_else(e.as_ref().map(|e| &**e))
          }
          // "another else-if-let"
          ast::ExprKind::IfLet(ref pat, ref expr, ref then, ref e) => {
            try!(self.cbox(INDENT_UNIT - 1));
            try!(self.ibox(0));
            try!(word(&mut self.s, " else if let "));
            try!(self.print_pat(&pat));
            try!(space(&mut self.s));
            try!(self.word_space("="));
            try!(self.print_expr(&expr));
            try!(space(&mut self.s));
            try!(self.print_block(&then));
            self.print_else(e.as_ref().map(|e| &**e))
          }
          // "final else"
          ast::ExprKind::Block(ref b) => {
            try!(self.cbox(INDENT_UNIT - 1));
            try!(self.ibox(0));
            try!(word(&mut self.s, " else "));
            self.print_block(&b)
          }
          // BLEAH, constraints would be great here
          _ => {
            panic!("print_if saw if with weird alternative");
          }
        }
      }
      _ => Ok(())
    }
  }

  fn print_expr_struct(&mut self,
                       path: &ast::Path,
                       fields: &[ast::Field],
                       wth: &Option<P<ast::Expr>>,
                       attrs: &[Attribute]) -> io::Result<()> {
    self.print_path(path, true, 0)?;
    word(&mut self.s, "{")?;
    //self.print_inner_attributes_inline(attrs)?;
    self.commasep_cmnt(
      Consistent,
      &fields[..],
      |s, field| {
        s.ibox(INDENT_UNIT)?;
        s.print_ident(field.ident.node)?;
        s.word_space(":")?;
        s.print_expr(&field.expr)?;
        s.end()
      },
      |f| f.span)?;
    match *wth {
      Some(ref expr) => {
        self.ibox(INDENT_UNIT)?;
        if !fields.is_empty() {
          word(&mut self.s, ",")?;
          space(&mut self.s)?;
        }
        word(&mut self.s, "..")?;
        self.print_expr(&expr)?;
        self.end()?;
      }
      _ => if !fields.is_empty() {
        word(&mut self.s, ",")?
      }
    }
    word(&mut self.s, "}")?;
    Ok(())
  }

  fn print_expr_tup(&mut self, exprs: &[P<ast::Expr>],
                    attrs: &[Attribute]) -> io::Result<()> {
    self.popen()?;
    //try!(self.print_inner_attributes_inline(attrs));
    self.commasep_exprs(Inconsistent, &exprs[..])?;
    if exprs.len() == 1 {
      word(&mut self.s, ",")?;
    }
    self.pclose()
  }

  fn print_expr_vec(&mut self, exprs: &[P<ast::Expr>],
                    attrs: &[Attribute]) -> io::Result<()> {
    self.ibox(INDENT_UNIT)?;
    word(&mut self.s, "[")?;
    //self.print_inner_attributes_inline(attrs)?;
    self.commasep_exprs(Inconsistent, &exprs[..])?;
    word(&mut self.s, "]")?;
    self.end()
  }

  fn print_expr_repeat(&mut self,
                       element: &ast::Expr,
                       count: &ast::Expr,
                       attrs: &[Attribute]) -> io::Result<()> {
    self.ibox(INDENT_UNIT)?;
    word(&mut self.s, "[")?;
    //self.print_inner_attributes_inline(attrs)?;
    self.print_expr(element)?;
    self.word_space(";")?;
    self.print_expr(count)?;
    word(&mut self.s, "]")?;
    self.end()
  }

  fn print_expr_call(&mut self,
                     func: &ast::Expr,
                     args: &[P<ast::Expr>]) -> io::Result<()> {
    self.print_expr_maybe_paren(func)?;
    self.print_call_post(args)
  }

  fn print_expr_method_call(&mut self,
                            ident: ast::SpannedIdent,
                            tys: &[P<ast::Ty>],
                            args: &[P<ast::Expr>]) -> io::Result<()> {
    let base_args = &args[1..];
    self.print_expr(&args[0])?;
    word(&mut self.s, ".")?;
    self.print_ident(ident.node)?;
    if !tys.is_empty() {
      word(&mut self.s, "::<")?;
      self.commasep(Inconsistent, tys,
                    |s, ty| s.print_type(&ty))?;
      word(&mut self.s, ">")?;
    }
    self.print_call_post(base_args)
  }

  fn print_call_post(&mut self, args: &[P<ast::Expr>]) -> io::Result<()> {
    self.popen()?;
    self.commasep_exprs(Inconsistent, args)?;
    self.pclose()
  }

  fn print_literal(&mut self, lit: &ast::Lit) -> io::Result<()> {
    try!(self.maybe_print_comment(lit.span.lo));
    match self.next_lit(lit.span.lo) {
      Some(ref ltrl) => {
        return word(self.writer(), &(*ltrl).lit);
      }
      _ => ()
    }
    match lit.node {
      ast::LitKind::Str(ref st, style) => self.print_string(&st, style),
      ast::LitKind::Byte(byte) => {
        let mut res = String::from("b'");
        res.extend(ascii::escape_default(byte).map(|c| c as char));
        res.push('\'');
        word(self.writer(), &res[..])
      }
      ast::LitKind::Char(ch) => {
        let mut res = String::from("'");
        res.extend(ch.escape_default());
        res.push('\'');
        word(self.writer(), &res[..])
      }
      ast::LitKind::Int(i, t) => {
        match t {
          ast::LitIntType::Signed(st) => {
            word(self.writer(),
                 &st.val_to_string(i as i64))
          }
          ast::LitIntType::Unsigned(ut) => {
            word(self.writer(), &ut.val_to_string(i))
          }
          ast::LitIntType::Unsuffixed => {
            word(self.writer(), &format!("{}", i))
          }
        }
      }
      ast::LitKind::Float(ref f, t) => {
        word(self.writer(),
             &format!(
               "{}{}",
               &f,
               t.ty_to_string()))
      }
      ast::LitKind::FloatUnsuffixed(ref f) => word(self.writer(), &f[..]),
      ast::LitKind::Bool(val) => {
        if val { word(self.writer(), "true") } else { word(self.writer(), "false") }
      }
      ast::LitKind::ByteStr(ref v) => {
        let mut escaped: String = String::new();
        for &ch in v.iter() {
          escaped.extend(ascii::escape_default(ch)
                             .map(|c| c as char));
        }
        word(self.writer(), &format!("b\"{}\"", escaped))
      }
    }
  }

  fn next_lit(&mut self, pos: BytePos) -> Option<comments::Literal> {
    let mut cur_lit = self.cur_cmnt_and_lit().cur_lit;

    let mut result = None;

    if let &Some(ref lits) = self.literals()
        {
          while cur_lit < lits.len() {
            let ltrl = (*lits)[cur_lit].clone();
            if ltrl.pos > pos { break; }
            cur_lit += 1;
            if ltrl.pos == pos {
              result = Some(ltrl);
              break;
            }
          }
        }

    self.cur_cmnt_and_lit().cur_lit = cur_lit;
    result
  }

  fn print_string(&mut self, st: &str,
                  style: ast::StrStyle) -> io::Result<()> {
    let st = match style {
      ast::StrStyle::Cooked => {
        (format!("\"{}\"", st.escape_default()))
      }
      ast::StrStyle::Raw(n) => {
        (format!("r{delim}\"{string}\"{delim}",
                 delim=repeat("#", n),
                 string=st))
      }
    };
    word(self.writer(), &st[..])
  }
}

pub fn visibility_qualified(vis: &ast::Visibility, s: &str) -> String {
  match *vis {
    ast::Visibility::Public => format!("pub {}", s),
    ast::Visibility::Inherited => s.to_string()
  }
}

fn needs_parentheses(expr: &ast::Expr) -> bool {
  match expr.node {
    ast::ExprKind::Assign(..) | ast::ExprKind::Binary(..) |
    ast::ExprKind::Closure |
    ast::ExprKind::AssignOp(..) | ast::ExprKind::Cast(..) |
    ast::ExprKind::InPlace(..) | ast::ExprKind::Type(..) => true,
    _ => false,
  }
}

pub fn path_to_string(p: &ast::Path) -> String {
  let path_str = p.segments.iter().join("::");

  if p.global {
    format!("::{}", &path_str)
  } else {
    path_str
  }
}

fn repeat(s: &str, n: usize) -> String { iter::repeat(s).take(n).collect() }