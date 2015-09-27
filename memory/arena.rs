//!
//! Arena for Variable Sized Data
//!

use alloc;
use alloc::heap;
use buffer::Buf;
use std::cmp;
use std::ptr;
use std::mem;

// only considered the memory aligned size of Intel x86
pub static DEFAULT_ALIGNED_SIZE: usize = 16;

#[derive(Copy, Clone)]
#[allow(raw_pointer_derive)]
struct Chunk {  
  ptr: *const u8,
  size: usize,
}

pub struct Arena {
  default_page_size: usize,
  aligned_size: usize,
  head: Chunk,
  offset: usize,  
  chunks: Vec<Chunk>,

  // stats
  allocated_size: usize,
  used_size: usize
}

impl Drop for Arena {
  fn drop(&mut self) {
    unsafe {
      // free head
      heap::deallocate(self.head.ptr as *mut u8, self.head.size, 
        self.aligned_size);

      // free all chunks
      for chunk in &*self.chunks {
        heap::deallocate(chunk.ptr as *mut u8, chunk.size, 
          self.aligned_size);
      }
    }
  }
}

impl Arena {

  pub fn new(page_size: usize) -> Arena {
    Arena::new_with_aligned(page_size, DEFAULT_ALIGNED_SIZE)
  }

  pub fn new_with_aligned(page_size: usize, aligned_size: usize) -> Arena {
    let allocated: *mut u8 = unsafe {
      heap::allocate(page_size, aligned_size) as *mut u8
    };

    Arena {
      default_page_size: page_size,
      aligned_size: aligned_size,
      head: Chunk{ptr: allocated, size: page_size},
      offset: 0,
      chunks: Vec::new(),

      allocated_size: page_size,
      used_size: 0
    }
  }

  pub fn chunk_num(&self) -> usize {
    self.chunks.len()
  }

  pub fn allocated_size(&self) -> usize {
    self.allocated_size
  }

  pub fn used_size(&self) -> usize {
    self.used_size + self.offset
  }

  pub fn alloc<'a>(&mut self, size: usize) -> Buf<'a> {
    if self.need_grow(size) {
      self.new_chunk(size);
    }

    let addr = self.head.ptr as usize + self.offset;    
    self.offset += size;
    let limit = addr + size;

    Buf::new(addr, limit)
  }

  /// Allocate a string 
  pub fn alloc_str(&mut self, str: &str) -> *const u8 {
    let bytes: &[u8] = unsafe {
      mem::transmute(str)
    };

    if self.need_grow(bytes.len()) {
      self.new_chunk(bytes.len());
    }

    let addr = (self.head.ptr as usize + self.offset) as *mut u8;
    unsafe { ptr::copy(bytes.as_ptr(), addr, bytes.len()); }
    self.offset += bytes.len();

    addr
  }

  fn need_grow(&self, size: usize) -> bool {
    (self.head.size - self.offset) < size
  }

  fn new_chunk(&mut self, required_size: usize) {
    let actual_size = cmp::max(self.default_page_size, required_size);

    let allocated: *mut u8 = unsafe {

      if self.offset == 0 {
        // because the current head chunk is deallocated
        self.allocated_size = self.allocated_size - self.head.size;

        heap::reallocate(
          self.head.ptr as *mut u8, 
          self.head.size, 
          actual_size, 
          self.aligned_size) as *mut u8
      } else {        
        self.chunks.push(self.head.clone());

        heap::allocate(
          actual_size, 
          self.aligned_size) as *mut u8
      }

    };

    if allocated.is_null() { alloc::oom() }

    // update stats
    self.used_size = self.used_size + self.offset;
    self.allocated_size = self.allocated_size + actual_size;

    // new head chunk
    self.offset = 0;
    self.head = Chunk{
      ptr: allocated, 
      size: actual_size, 
    };
  }
}

#[allow(unused_imports)]
use bytesize::ByteSize;
#[allow(unused_imports)]
use buffer::{UnSafeDatumWriter, UnSafeDatumReader};

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

  buf.write_i32(5);
}

#[test]
#[allow(unused_variables)] 
fn test_many_chunks() {
  let mut arena = Arena::new(ByteSize::kb(4).as_usize());

  for i in 0..1024 {
    arena.alloc(ByteSize::kb(2).as_usize());
  }

  // 1 head + 1023
  assert_eq!(arena.chunk_num(), 511);
  assert_eq!(arena.allocated_size(), ByteSize::kb(2048).as_usize());
}

#[test]
#[allow(unused_variables)] 
fn test_large_chunks() {
  let mut arena = Arena::new(ByteSize::kb(4).as_usize());

  for i in 0..1024 {
    arena.alloc(ByteSize::kb(8).as_usize());
  }

  // 1 head + 1023
  assert_eq!(arena.chunk_num(), 1023);
  assert_eq!(arena.allocated_size(), ByteSize::kb(8 * 1024).as_usize());
}