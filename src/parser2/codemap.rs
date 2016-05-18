use std::ops::{Add, Sub};

/// A byte offset. Keep this small (currently 32-bits), as AST contains
/// a lot of them.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct BytePos(pub u32);

impl BytePos {
  pub fn from_usize(n: usize) -> BytePos { BytePos(n as u32) }
  pub fn to_usize(&self) -> usize { let BytePos(n) = *self; n as usize }
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

/// Spans represent a region of code, used for error reporting. Positions in spans
/// are *absolute* positions from the beginning of the codemap.
#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub struct Span {
  pub lo: BytePos,
  pub hi: BytePos
}

pub const DUMMY_SP: Span = Span { lo: BytePos(0), hi: BytePos(0) };