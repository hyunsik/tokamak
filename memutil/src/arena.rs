use alloc;
use alloc::heap;
use buffer::Buf;
use std::cell::RefCell;
use std::cmp;

#[derive(Copy, Clone)]
#[allow(raw_pointer_derive)]
struct Chunk {  
  ptr: *const u8,
}

pub struct Arena {
  page_size: usize,  
  chunk: RefCell<Chunk>,
  offset: usize,  
  chunks: Vec<Chunk>
}

// impl Drop for Arena {
//   fn drop(&mut self) {
//     unsafe {
//       heap::deallocate(self.chunk.borrow().ptr as *mut u8, self.page_size, 16);

//       for chunk in &*self.chunks {
//         heap::deallocate(chunk.ptr as *mut u8, self.page_size, 16);
//       }
//     }
//   }
// }

impl Arena {
  pub fn new(page_size: usize) -> Arena {

    let allocated: *mut u8 = unsafe {
      heap::allocate(page_size, 16) as *mut u8
    };

    Arena {
      page_size: page_size,
      chunk: RefCell::new(Chunk{ptr: allocated}),
      offset: 0,
      chunks: Vec::new()
    }
  }

  pub fn alloc(&mut self, size: usize) -> Buf {
    if !self.ensure_alloc(size) {
      self.new_chunk(size);
    }

    let addr = self.chunk.borrow().ptr as usize;
    let offset = addr + size;
    Buf::new(addr, offset)
  }

  // pub fn alloc_str(&mut self, str: &str) -> const *u8 {

  // }

  fn ensure_alloc(&self, size: usize) -> bool {
    (self.page_size - self.offset) <= size
  }

  fn new_chunk(&mut self, required_size: usize) {
    let allocated: *mut u8 = unsafe {
      heap::allocate(cmp::min(self.page_size, required_size), 16) as *mut u8
    };

    if allocated.is_null() { alloc::oom() }

    self.offset = 0;
    self.chunks.push(self.chunk.borrow().clone());        
    *self.chunk.borrow_mut() = Chunk{ptr: allocated};
  }
}