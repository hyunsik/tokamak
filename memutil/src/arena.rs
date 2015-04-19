use alloc;
use alloc::heap;
use buffer::Buf;
use std::cmp;

#[derive(Copy, Clone)]
#[allow(raw_pointer_derive)]
struct Chunk {  
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
      self.new_chunk(size);
    }

    let addr = self.chunk.ptr as usize;
    let offset = addr + size;
    Buf::new(addr, offset)
  }

  fn ensure_alloc(&self, size: usize) -> bool {
    (self.page_size - self.offset) <= size
  }

  fn new_chunk(&mut self, required_size: usize) {


    let allocated: *mut u8 = unsafe {
      heap::allocate(cmp::min(self.page_size, required_size), 16) as *mut u8
    };

    if allocated.is_null() { alloc::oom() }

    let last_chunk = self.chunk;
    self.offset = 0;
    self.chunks.push(last_chunk);        
    self.chunk = Chunk{ptr: allocated};
  }
}