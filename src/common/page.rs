/// This row implementation is basically based on Pax,
/// but it has a variation in terms of variable-length blocks.
///
/// ## Deign Consideration
///
/// The API is being designed with the following design consideration:
///
/// * Reuseable allocated memory
/// * Vectorized processing
/// * Various encodings (light/heavy weight compression and no
///   deserialization from storage pages)
/// * Late materialization (Refer to [3])
///
/// ## References
/// * [1] Daniel Abadi et al., The Design and Implementation of Modern Column-Oriented Database
///       Systems
/// * [2]
/// * [3] Daniel J. Abadi ea al., Materialization Strategies in a Column-Oriented DBMS, ICDE 2007

use alloc::heap;
use std::fmt;
use std::ptr;

use platform::get_aligned_size;
use types::Ty;

/// Each executor and operator process a batch of rows at a time for better throughput.
/// The experiment of MonetDB presented that 1024 is the best number of a row batch.
/// It's reason why I currently use 1024 as the number of row batch.
pub const ROWBATCH_SIZE: usize = 1024;
pub const ALIGNED_SIZE: usize = 16;

/// Encoding Type for Chunk
///
/// * RAW - No encoding and raw byte representation
/// * RLE - Run length encoding
#[repr(C)]
#[derive(Clone)]
pub enum EncType {
  RAW = 0,
  RLE = 1,
}

/// Memory Chunk
///
/// It is usually a slice to point the memory area.
#[repr(C)]
#[derive(Copy, Clone)]
pub struct Chunk {
  pub ptr: *const u8,
  pub size: usize,
  // pub owned: false
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct Run {
  pub length: u8,
  pub value: *const u8,
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct RLEChunk {
  pub run_num: i16,
  pub runs: *const Run,
}

impl fmt::Display for Chunk {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "Chunk {{ptr:{}, size:{}}}", self.ptr as usize, self.size)
  }
}

pub struct RawChunkWriter;

#[repr(C)]
#[derive(Clone)]
pub struct Page {
  pub ptr: *const u8,
  pub size: usize,

  pub chunks: *const Chunk,
  /// the number of Chunks
  pub chunk_num: usize,
  // the number of values stored in each Chunk.
  // All Chunks share the same val_cnt.
  pub value_cnt: usize,
  // Does this page own the Chunks?
  pub owned: bool,
}

impl fmt::Display for Page {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f,
           "Page {{ptr:{}, size:{}, chunk_num:{}, value_cnt:{}, owned:{}}}",
           self.ptr as usize,
           self.size,
           self.chunk_num,
           self.value_cnt,
           self.owned)
  }
}

impl Drop for Page {
  fn drop(&mut self) {
    // to deallocate vector
    let vec: Vec<Chunk> = unsafe {
      Vec::from_raw_parts(self.chunks as *mut Chunk, self.chunk_num, self.chunk_num)
    };

    // only if owned page will be deallocated
    if self.owned {
      unsafe { heap::deallocate(self.ptr as *mut u8, self.size, ALIGNED_SIZE) };
    }
  }
}

// Get a chunk size according to both type and encoding type.
fn compute_chunk_size(ty: &Ty, enc: &EncType) -> usize {
  // TODO: consider the given encoding type.
  get_aligned_size(ty.size_of() * ROWBATCH_SIZE)
}

impl Page {
  pub fn empty_page(num: usize) -> Page {
    let mut empty_chunks: Vec<Chunk> = ::std::iter::repeat(unsafe { ::std::mem::uninitialized() })
                                         .take(num)
                                         .collect::<Vec<Chunk>>();
    empty_chunks.shrink_to_fit();

    let page = Page {
      ptr: ::std::ptr::null(),
      size: 0,

      chunks: empty_chunks.as_ptr(),
      value_cnt: 0,
      chunk_num: num,
      owned: false,
    };
    ::std::mem::forget(empty_chunks);

    page
  }

  pub fn set_chunks(&mut self, chunks: &[&Chunk]) {
    assert!(self.owned == false,
            "Owned page does not support Page::set().");
    debug_assert!(chunks.len() == self.chunk_num,
                  "The number of chunks must be the same to that of the page.");

    let mut total_sz = 0;
    for (idx, each_chunk) in izip!(0..chunks.len(), chunks) {
      total_sz += each_chunk.size;
      self.chunks_mut()[idx] = **each_chunk;
    }
    self.size = total_sz;
  }

  pub fn new(types: &[&Ty], encs: Option<&[EncType]>) -> Page {
    match encs {
      Some(e) => Page::new_with_enc(types, e),
      None => {
        let raw_encs = ::std::iter::repeat(EncType::RAW)
                         .take(types.len())
                         .collect::<Vec<EncType>>();
        Page::new_with_enc(types, &raw_encs[..])
      }
    }
  }

  fn new_with_enc(types: &[&Ty], encs: &[EncType]) -> Page {
    // Precondition
    debug_assert!(types.len() == encs.len(),
                  "num of types and encodings must be equal.");

    // extract fixed columns
    // allocate memory for fixed columns
    // assign pointers to chunks

    let mem_sz = get_aligned_size(izip!(types, encs)
                                    .map(|(t, e)| compute_chunk_size(t, e))
                                    .fold(0, |acc, sz| acc + sz));

    let mut ptr = unsafe { heap::allocate(mem_sz, ALIGNED_SIZE) };
    let chunks = Page::create_chunks(ptr, mem_sz, types, encs);

    let page = Page {
      ptr: ptr,
      size: mem_sz,

      chunks: chunks.as_ptr(),
      chunk_num: types.len(),
      value_cnt: 0usize,
      owned: true,
    };
    // forget Vec to keep raw pointer in Page.
    // It is necessary in order to make Page compatible with LLVM IR.
    ::std::mem::forget(chunks);

    page
  }

  /// Create chunks for data types and encoding types
  fn create_chunks(ptr: *const u8, area_sz: usize, types: &[&Ty], encs: &[EncType]) -> Vec<Chunk> {

    let mut acc: usize = 0;
    let mut chunks: Vec<Chunk> = Vec::with_capacity(types.len());

    for (t, e) in izip!(types, encs) {
      let sz = compute_chunk_size(t, e);
      chunks.push(Chunk {
        ptr: unsafe { ptr.offset(acc as isize) },
        size: sz,
      });
      acc += sz;
    }

    // validation
    debug_assert!(acc == area_sz);
    debug_assert!(types.len() == chunks.len());
    debug_assert!(chunks.len() == chunks.capacity());

    chunks
  }

  pub fn size(&self) -> usize {
    self.size
  }

  pub fn chunks<'a>(&'a self) -> &'a [Chunk] {
    unsafe { ::std::slice::from_raw_parts(self.chunks, self.chunk_num) }
  }

  pub fn chunks_mut<'a>(&'a self) -> &'a mut [Chunk] {
    unsafe { ::std::slice::from_raw_parts_mut(self.chunks as *mut Chunk, self.chunk_num) }
  }

  pub fn chunk<'a>(&'a self, page_id: usize) -> &'a Chunk {
    let ms: &[Chunk] = unsafe { ::std::slice::from_raw_parts(self.chunks, self.chunk_num) };
    &ms[page_id]
  }

  pub fn chunk_ptr(&self, page_id: usize) -> *const Chunk {
    let ms: &[Chunk] = unsafe { ::std::slice::from_raw_parts(self.chunks, self.chunk_num) };
    &ms[page_id]
  }

  pub fn chunk_num(&self) -> usize {
    self.chunk_num
  }

  pub fn set_value_count(&mut self, cnt: usize) {
    self.value_cnt = cnt
  }

  pub fn value_count(&self) -> usize {
    self.value_cnt
  }

  pub fn copy(&self) -> Page {
    if self.owned {
      self.copy_owned()
    } else {
      self.copy_not_owned()
    }
  }

  fn copy_owned(&self) -> Page {
    let mut ptr = unsafe { heap::allocate(self.size, ALIGNED_SIZE) };
    unsafe {
      ptr::copy_nonoverlapping(self.ptr, ptr, self.size);
    }

    let mut chunks = Vec::new();
    let mut acc = 0;
    for c in self.chunks() {
      chunks.push(Chunk {
        ptr: unsafe { ptr.offset(acc as isize) },
        size: c.size,
      });
      acc += c.size;
    }

    let page = Page {
      ptr: ptr,
      size: self.size,

      chunks: chunks.as_ptr(),
      chunk_num: self.chunk_num,
      value_cnt: self.value_cnt,
      owned: true,
    };
    // forget Vec to keep raw pointer in Page.
    // It is necessary in order to make Page compatible with LLVM IR.
    ::std::mem::forget(chunks);

    page
  }

  fn copy_not_owned(&self) -> Page {
    // if still empty, just return its copy.
    if self.size == 0 {
      return self.clone();
    }

    let mut ptr = unsafe { heap::allocate(self.size, ALIGNED_SIZE) };

    let mut chunks = Vec::new();
    let mut acc = 0;

    for src_chunk in self.chunks() {
      let chunk_ptr = unsafe { ptr.offset(acc as isize) };
      unsafe {
        ptr::copy_nonoverlapping(src_chunk.ptr, chunk_ptr, src_chunk.size);
      }
      chunks.push(Chunk {
        ptr: chunk_ptr,
        size: src_chunk.size,
      });

      acc += src_chunk.size;
    }

    let page = Page {
      ptr: ptr,
      size: self.size,

      chunks: chunks.as_ptr(),
      chunk_num: self.chunk_num,
      value_cnt: self.value_cnt,
      owned: true,
    };

    // forget Vec to keep raw pointer in Page.
    // It is necessary in order to make Page compatible with LLVM IR.
    ::std::mem::forget(chunks);

    page
  }

  pub fn project<'a>(&'a self, ids: &[usize]) -> Vec<&'a Chunk> {
    ids.iter()
       .map(|i| self.chunk(*i))
       .collect::<Vec<&Chunk>>()
  }
}

pub mod c_api {
  use super::{Chunk, EncType, Page, RLEChunk};
  use types::Ty;

  pub static FN_GET_RAW_CHUNK: &'static str = "get_raw_chunk";
  pub static FN_GET_RLE_CHUNK: &'static str = "get_rle_chunk";

  pub static FN_READ_BOOL_RAW: &'static str = "read_i8_raw";
  pub static FN_READ_I8_RAW: &'static str = "read_i8_raw";
  pub static FN_READ_I16_RAW: &'static str = "read_i16_raw";
  pub static FN_READ_I32_RAW: &'static str = "read_i32_raw";
  pub static FN_READ_I64_RAW: &'static str = "read_i64_raw";
  pub static FN_READ_F32_RAW: &'static str = "read_f32_raw";
  pub static FN_READ_F64_RAW: &'static str = "read_f64_raw";

  pub static FN_READ_I8_RLE: &'static str = "read_i8_rle";
  pub static FN_READ_I16_RLE: &'static str = "read_i16_rle";
  pub static FN_READ_I32_RLE: &'static str = "read_i32_rle";
  pub static FN_READ_I64_RLE: &'static str = "read_i64_rle";
  pub static FN_READ_F32_RLE: &'static str = "read_f32_rle";
  pub static FN_READ_F64_RLE: &'static str = "read_f64_rle";

  pub fn fn_name_of_get_chunk(enc_type: &EncType) -> &'static str {
    match *enc_type {
      EncType::RAW => FN_GET_RAW_CHUNK,
      EncType::RLE => FN_GET_RLE_CHUNK,
    }
  }

  pub fn fn_name_of_read_raw(ty: &Ty) -> &'static str {
    match *ty {
      Ty::Bool => FN_READ_BOOL_RAW,
      Ty::I8 => FN_READ_I8_RAW,
      Ty::I16 => FN_READ_I16_RAW,
      Ty::I32 => FN_READ_I32_RAW,
      Ty::I64 => FN_READ_I64_RAW,
      Ty::F32 => FN_READ_F32_RAW,
      Ty::F64 => FN_READ_F64_RAW,
      _ => panic!("not supported type"),
    }
  }

  pub fn fn_name_of_read_rle(ty: &Ty) -> &'static str {
    match *ty {
      Ty::I8 => FN_READ_I8_RLE,
      Ty::I16 => FN_READ_I16_RLE,
      Ty::I32 => FN_READ_I32_RLE,
      Ty::I64 => FN_READ_I64_RLE,
      Ty::F32 => FN_READ_F32_RLE,
      Ty::F64 => FN_READ_F64_RLE,
      _ => panic!("not supported type"),
    }
  }

  extern "C" {
    pub fn get_raw_chunk(p: *const Page, idx: usize) -> *const Chunk;
    pub fn get_rle_chunk(p: *const Page, idx: usize) -> *const RLEChunk;

    pub fn write_i8_raw(p: *const Chunk, idx: usize, val: i8);
    pub fn write_i16_raw(p: *const Chunk, idx: usize, val: i16);
    pub fn write_i32_raw(p: *const Chunk, idx: usize, val: i32);
    pub fn write_i64_raw(p: *const Chunk, idx: usize, val: i64);
    pub fn write_f32_raw(p: *const Chunk, idx: usize, val: f32);
    pub fn write_f64_raw(p: *const Chunk, idx: usize, val: f64);

    pub fn read_i8_raw(p: *const Chunk, idx: usize) -> i8;
    pub fn read_i16_raw(p: *const Chunk, idx: usize) -> i16;
    pub fn read_i32_raw(p: *const Chunk, idx: usize) -> i32;
    pub fn read_i64_raw(p: *const Chunk, idx: usize) -> i64;
    pub fn read_f32_raw(p: *const Chunk, idx: usize) -> f32;
    pub fn read_f64_raw(p: *const Chunk, idx: usize) -> f64;

    pub fn read_i8_rle(p: *const RLEChunk, idx: usize) -> i8;
    pub fn read_i16_rle(p: *const RLEChunk, idx: usize) -> i16;
    pub fn read_i32_rle(p: *const RLEChunk, idx: usize) -> i32;
    pub fn read_i64_rle(p: *const RLEChunk, idx: usize) -> i64;
    pub fn read_f32_rle(p: *const RLEChunk, idx: usize) -> f32;
    pub fn read_f64_rle(p: *const RLEChunk, idx: usize) -> f64;

    // for test
    pub fn random_rle_chunk() -> RLEChunk;
  }
}
