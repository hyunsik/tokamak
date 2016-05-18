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

pub const DUMMY_SP: Span = Span { lo: BytePos(0), hi: BytePos(0) };