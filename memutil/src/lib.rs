#![feature(alloc)]
extern crate alloc;

use alloc::heap;
use std::mem;
use std::ops::{Add,Sub,Mul,Div};

pub static B: u64 = 1;
pub static KB: u64 = 1000;
pub static MB: u64 = 1000000;
pub static GB: u64 = 1000000000;
pub static TB: u64 = 1000000000000;
pub static PB: u64 = 1000000000000000;

#[derive(Debug)]
struct ByteSize {
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
  pub fn mb(size: u64) -> ByteSize {
    ByteSize {size: size * MB}
  }

  #[inline(always)]
  pub fn gb(size: u64) -> ByteSize {
    ByteSize {size: size * GB}
  }

  #[inline(always)]
  pub fn tb(size: u64) -> ByteSize {
    ByteSize {size: size * TB}
  }

  #[inline(always)]
  pub fn pb(size: u64) -> ByteSize {
    ByteSize {size: size * PB}
  }

  #[inline(always)]
  pub fn as_u64(self) -> u64 {
    self.size
  }

  #[inline(always)]
  pub fn as_usize(self) -> usize {
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
    ByteSize {size: (self.size + rhs)}
  }
}

impl Sub<ByteSize> for ByteSize {
  type Output = ByteSize;

  #[inline(always)]
  fn sub(self, rhs: ByteSize) -> ByteSize {
    ByteSize {size: (self.size + rhs.size)}
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

#[derive(Debug, Clone, Copy)]
pub struct Buf {
  ptr: usize,
  limit: usize
}

trait UnSafeDatumWriter {
  fn write_bool(&mut self, value: bool);
  fn write_i8  (&mut self, value: i8);
  fn write_i16 (&mut self, value: i16);
  fn write_i32 (&mut self, value: i32);
  fn write_i64 (&mut self, value: i64);
  fn write_f32 (&mut self, value: f32);
  fn write_f64 (&mut self, value: f64);
}

trait UnSafeDatumReader {
  fn read_bool(&mut self) -> bool;
  fn read_i8  (&mut self) -> i8;
  fn read_i16 (&mut self) -> i16;
  fn read_i32 (&mut self) -> i32;
  fn read_i64 (&mut self) -> i64;
  fn read_f32 (&mut self) -> f32;
  fn read_f64 (&mut self) -> f64;
}

impl UnSafeDatumWriter for Buf {

  fn write_bool(&mut self, value: bool) {
    debug_assert!((self.ptr + mem::size_of::<i8>()) <= self.limit, 
      "buffer overrun");

    unsafe {
      *(self.ptr as *mut i8) = value as i8;
    }

    self.ptr += mem::size_of::<i8>();
  }
  fn write_i8  (&mut self, value: i8) {
    debug_assert!((self.ptr + mem::size_of::<i8>()) <= self.limit, 
      "buffer overrun");

    unsafe {
      *(self.ptr as *mut i8) = value;
    }

    self.ptr += mem::size_of::<i8>();
  }

  fn write_i16 (&mut self, value: i16) {
    debug_assert!((self.ptr + mem::size_of::<i16>()) <= self.limit, 
      "buffer overrun");

    unsafe {
      *(self.ptr as *mut i16) = value;
    }

    self.ptr += mem::size_of::<i16>();
  }

  fn write_i32 (&mut self, value: i32) {
    debug_assert!((self.ptr + mem::size_of::<i32>()) <= self.limit, 
      "buffer overrun");

    unsafe {
      *(self.ptr as *mut i32) = value;
    }

    self.ptr += mem::size_of::<i32>();
  }

  fn write_i64 (&mut self, value: i64) {
    debug_assert!((self.ptr + mem::size_of::<i64>()) <= self.limit, 
      "buffer overrun");

    unsafe {
      *(self.ptr as *mut i64) = value as i64;
    }

    self.ptr += mem::size_of::<i64>();
  }

  fn write_f32 (&mut self, value: f32) {
    debug_assert!((self.ptr + mem::size_of::<f32>()) <= self.limit, 
      "buffer overrun");

    unsafe {
      *(self.ptr as *mut f32) = value;
    }

    self.ptr += mem::size_of::<i32>();
  }

  fn write_f64 (&mut self, value: f64) {
    debug_assert!((self.ptr + mem::size_of::<f64>()) <= self.limit, 
      "buffer overrun");

    unsafe {
      *(self.ptr as *mut f64) = value;
    }

    self.ptr += mem::size_of::<f64>();
  }
}

impl UnSafeDatumReader for Buf {
  fn read_bool(&mut self) -> bool {
    debug_assert!((self.ptr + mem::size_of::<i8>()) <= self.limit, 
      "buffer overrun");
    
    let ptr = self.ptr;
    self.ptr += mem::size_of::<i8>();

    unsafe {
      *(ptr as *const bool)
    }
  }

  fn read_i8(&mut self) -> i8 {
    debug_assert!((self.ptr + mem::size_of::<i8>()) <= self.limit, 
      "buffer overrun");

    let ptr = self.ptr;
    self.ptr += mem::size_of::<i8>();

    unsafe {
      *(ptr as *const i8)
    }
  }
  fn read_i16(&mut self) -> i16 {
    debug_assert!((self.ptr + mem::size_of::<i8>()) <= self.limit, 
      "buffer overrun");

    let ptr = self.ptr;
    self.ptr += mem::size_of::<i16>();

    unsafe {
      *(ptr as *const i16)
    }
  }
  fn read_i32(&mut self) -> i32 {
    debug_assert!((self.ptr + mem::size_of::<i8>()) <= self.limit, 
      "buffer overrun");

    let ptr = self.ptr;
    self.ptr += mem::size_of::<i32>();

    unsafe {
      *(ptr as *const i32)
    }
  }
  fn read_i64(&mut self) -> i64 {
    debug_assert!((self.ptr + mem::size_of::<i8>()) <= self.limit, 
      "buffer overrun");

    let ptr = self.ptr;
    self.ptr += mem::size_of::<i64>();

    unsafe {
      *(ptr as *const i64)
    }
  }
  fn read_f32(&mut self) -> f32 {
    debug_assert!((self.ptr + mem::size_of::<i8>()) <= self.limit, 
      "buffer overrun");

    let ptr = self.ptr;
    self.ptr += mem::size_of::<f32>();

    unsafe {
      *(ptr as *const f32)
    }
  }
  fn read_f64 (&mut self) -> f64 {
    debug_assert!((self.ptr + mem::size_of::<i8>()) <= self.limit, 
      "buffer overrun");

    let ptr = self.ptr;
    self.ptr += mem::size_of::<f64>();

    unsafe {
      *(ptr as *const f64)
    }
  }
}

#[derive(Copy, Clone)]
struct Chunk {
  #[allow(raw_pointer_derive)]
  ptr: *const u8,
}

pub struct Arena {
  page_size: usize,  
  chunk: Chunk,
  offset: usize,  
  chunks: Vec<Chunk>
}

impl Arena {
  pub fn new(page_size: usize) -> Arena {

    let allocated: *mut u8 = unsafe {
      heap::allocate(page_size, 16) as *mut u8
    };

    let first_chunk = Chunk{ptr: allocated};

    Arena {
      page_size: page_size,
      chunk: first_chunk,
      offset: 0,
      chunks: Vec::new()
    }
  }

  pub fn alloc(&mut self, size: usize) -> Buf {
    if !self.ensure_alloc(size) {
      self.new_chunk();
    }

    let addr = self.chunk.ptr as usize;
    let offset = addr + size;
    Buf {ptr: addr, limit: offset}
  }

  fn ensure_alloc(&self, size: usize) -> bool {
    (self.page_size - self.offset) <= size
  }

  fn new_chunk(&mut self) {

    let allocated: *mut u8 = unsafe {
      heap::allocate(self.page_size, 16) as *mut u8
    };

    if allocated.is_null() { alloc::oom() }

    let last_chunk = self.chunk;
    self.offset = 0;
    self.chunks.push(last_chunk);        
    self.chunk = Chunk{ptr: allocated};
  }
}

#[test]
fn test_buf_read_write() {
  let mut arena = Arena::new(ByteSize::kb(16).as_usize());
  let mut buf = arena.alloc(ByteSize::kb(1).as_usize());
  let mut copy = buf;

  buf.write_bool(true);
  buf.write_i8(1);
  buf.write_i16(9);
  buf.write_i32(8);
  buf.write_i64(0);
  buf.write_f32(4.0);
  buf.write_f64(1.0);

  assert_eq!(copy.read_bool(), true);
  assert_eq!(copy.read_i8(), 1);
  assert_eq!(copy.read_i16(), 9);
  assert_eq!(copy.read_i32(), 8);
  assert_eq!(copy.read_i64(), 0);
  assert_eq!(copy.read_f32(), 4 as f32);
  assert_eq!(copy.read_f64(), 1 as f64);
}

#[test] 
#[should_panic(expected = "buffer overrun")]
fn test_write_buf_overrun() {
  let mut arena = Arena::new(ByteSize::kb(16).as_usize());
  let mut buf = arena.alloc(ByteSize::b(16).as_usize());

  buf.write_i32(1);
  buf.write_i32(2);
  buf.write_i32(3);
  buf.write_i32(4);
  buf.write_i32(4); // this write exceeds allocated buffer size
}