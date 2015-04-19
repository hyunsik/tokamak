use std::ops::{Add,Sub,Mul,Div};

pub static B: u64 = 1;
pub static KB: u64 = 1000;
pub static MB: u64 = 1000000;
pub static GB: u64 = 1000000000;
pub static TB: u64 = 1000000000000;
pub static PB: u64 = 1000000000000000;

pub static KIB: u64 = 1024;
pub static MIB: u64 = 1048576;
pub static GIB: u64 = 1073741824;
pub static TIB: u64 = 1099511627776;
pub static PIB: u64 = 1125899906842624;

#[derive(Debug, Copy, Clone)]
pub struct ByteSize {
  size: u64
}

impl ByteSize {
  #[inline(always)]
  pub fn b(size: u64) -> ByteSize {
    ByteSize {size: size}
  }

  #[inline(always)]
  pub fn kb(size: u64) -> ByteSize {
    ByteSize {size: size * KB}
  }

  #[inline(always)]
  pub fn kib(size: u64) -> ByteSize {
   ByteSize {size: size * KIB} 
  }

  #[inline(always)]
  pub fn mb(size: u64) -> ByteSize {
    ByteSize {size: size * MB}
  }

  #[inline(always)]
  pub fn mib(size: u64) -> ByteSize {
    ByteSize {size: size * MIB}
  }

  #[inline(always)]
  pub fn gb(size: u64) -> ByteSize {
    ByteSize {size: size * GB}
  }

  #[inline(always)]
  pub fn gib(size: u64) -> ByteSize {
    ByteSize {size: size * GIB}
  }

  #[inline(always)]
  pub fn tb(size: u64) -> ByteSize {
    ByteSize {size: size * TB}
  }

  #[inline(always)]
  pub fn tib(size: u64) -> ByteSize {
    ByteSize {size: size * TIB}
  }

  #[inline(always)]
  pub fn pb(size: u64) -> ByteSize {
    ByteSize {size: size * PB}
  }

  #[inline(always)]
  pub fn pib(size: u64) -> ByteSize {
    ByteSize {size: size * PIB}
  }

  #[inline(always)]
  pub fn as_u64(&self) -> u64 {
    self.size
  }

  #[inline(always)]
  pub fn as_usize(&self) -> usize {
    self.size as usize
  }
}

impl Add<u64> for ByteSize {
  type Output = ByteSize;

  #[inline(always)]
  fn add(self, rhs: u64) -> ByteSize {
    ByteSize {size: (self.size + rhs)}
  }
}

impl Add<ByteSize> for ByteSize {
  type Output = ByteSize;

  #[inline(always)]
  fn add(self, rhs: ByteSize) -> ByteSize {
    ByteSize {size: (self.size + rhs.size)}
  }
}

impl Sub<u64> for ByteSize {
  type Output = ByteSize;

  #[inline(always)]
  fn sub(self, rhs: u64) -> ByteSize {
    ByteSize {size: (self.size - rhs)}
  }
}

impl Sub<ByteSize> for ByteSize {
  type Output = ByteSize;

  #[inline(always)]
  fn sub(self, rhs: ByteSize) -> ByteSize {
    ByteSize {size: (self.size - rhs.size)}
  }
}

impl Mul<u64> for ByteSize {
  type Output = ByteSize;

  #[inline(always)]
  fn mul(self, rhs: u64) -> ByteSize {
    ByteSize {size: (self.size * rhs)}
  }
}

impl Div<u64> for ByteSize {
  type Output = ByteSize;

  #[inline(always)]
  fn div(self, rhs: u64) -> ByteSize {
    ByteSize {size: (self.size / rhs)}
  }
}