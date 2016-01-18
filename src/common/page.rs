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
use std::marker;
use std::ptr;
use std::slice;

use platform::get_aligned_size;
use types::Ty;

/// Each executor and operator process a batch of rows at a time for better throughput.
/// The experiment of MonetDB presented that 1024 is the best number of a row batch.
/// It's reason why I currently use 1024 as the number of row batch.
pub static ROWBATCH_SIZE: usize = 1024;
pub static ALIGNED_SIZE: usize = 16;

/// Encoding Type for Chunk
///
/// * RAW - No encoding and raw byte representation
/// * RLE - Run length encoding
#[repr(C)] #[derive(Clone)]
pub enum EncType {
  RAW = 0,
  RLE = 1
}

/// Memory Chunk
///
/// It is usually a slice to point the memory area.
#[repr(C)] #[derive(Clone)]
pub struct Chunk {
  pub ptr : *const u8,
  pub size: usize,
  //pub owned: false
}


impl fmt::Display for Chunk {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "Chunk {{ptr:{}, size:{}}}", self.ptr as usize, self.size)
  }
}

/*
impl Chunk {
  pub fn new(elem_size: usize) -> Chunk {
    let required_size = get_aligned_size(elem_size * ROWBATCH_SIZE);
    let ptr = unsafe { heap::allocate(required_size, ALIGNED_SIZE) };

    Chunk {
      ptr : ptr,
      size: required_size,
    }
  }

  pub fn to_owned(&self) -> Chunk {
  	let mut ptr = unsafe { heap::allocate(self.size, ALIGNED_SIZE) };
    unsafe { ptr::copy_nonoverlapping(self.ptr, ptr, self.size); }

  	Chunk {
      ptr: ptr,
      size: self.size
    }
  }
}
*/

pub struct RawChunkWriter;

#[repr(C)]
pub struct Page {
  pub ptr        : *const u8,
  pub size       : usize,

  pub chunks     : *const Chunk,
  /// the number of Chunks
  pub chunk_num  : usize,
  // the number of values stored in each Chunk.
  // All Chunks share the same val_cnt.
  pub value_cnt  : usize,
  // Does this page own the Chunks?
  pub owned      : bool
}

impl fmt::Display for Page {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "Page {{ptr:{}, size:{}, chunk_num:{}, value_cnt:{}, owned:{}}}", 
      self.ptr as usize, self.size, self.chunk_num, self.value_cnt, self.owned)
  }
}

impl Drop for Page {
  fn drop(&mut self) {
    // to deallocate vector
    let vec:Vec<Chunk> = unsafe {
      Vec::from_raw_parts(self.chunks as *mut Chunk, self.chunk_num, self.chunk_num)
    };

    // only if owned page will be deallocated
    if self.owned {
      unsafe { heap::deallocate(self.ptr as *mut u8, self.size, ALIGNED_SIZE) };
    }
  }
}

/// Get a chunk size according to both type and encoding type.
fn compute_chunk_size(ty: &Ty, enc: &EncType) -> usize {
  get_aligned_size(ty.size_of() * ROWBATCH_SIZE)
}

impl Page {

  pub fn new(types: &[&Ty], encs: Option<&[EncType]>) -> Page {
    match encs {
      Some(e) => Page::new_with_enc(types, e),
      None    => {
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

    let total_sz = izip!(types, encs)
      .map(|(t,e)| compute_chunk_size(t, e))
      .fold(0, |acc, sz| acc + sz);

    let mut ptr = unsafe { heap::allocate(total_sz, ALIGNED_SIZE) };
    let chunks = Page::create_chunks(ptr, types, encs);

    let page = Page {
      ptr      : ptr,
      size     : total_sz,

      chunks   : chunks.as_ptr(),
      chunk_num: types.len(),
      value_cnt: 0usize,
      owned    : true
    };
    // forget Vec to keep raw pointer in Page.
    // It is necessary in order to make Page compatible with LLVM IR.
    ::std::mem::forget(chunks);

    page
  }

  /// Create chunks for data types and encoding types
  fn create_chunks(ptr: *const u8, types: &[&Ty], encs: &[EncType]) -> Vec<Chunk> {

    let mut acc: usize = 0;
    let mut chunks: Vec<Chunk> = Vec::with_capacity(types.len());

    for (t,e) in izip!(types, encs) {
      let sz = compute_chunk_size(t, e);
      acc += sz;

      let cur_chunk = Chunk {
        ptr: unsafe { ptr.offset(acc as isize) },
        size: sz
      };

      chunks.push(cur_chunk);
    }

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

  pub fn chunk_num(&self) -> usize { self.chunk_num }

  pub fn set_value_count(&mut self, cnt: usize) { self.value_cnt = cnt }

  pub fn value_count(&self) -> usize { self.value_cnt }

  pub fn project<'a>(&'a self, ids: &[usize]) -> Vec<&'a Chunk> {
  	ids.iter()
  		.map(|i| self.chunk(*i))
  		.collect::<Vec<&Chunk>>()
  }
}

pub mod c_api {
  use super::{Chunk, Page};

  extern "C" {
    pub fn get_chunk(p: *const Page, idx: usize) -> *const Chunk;

    pub fn write_raw_i8 (p: *const Chunk, idx: usize, val: i8);
    pub fn write_raw_i16(p: *const Chunk, idx: usize, val: i16);
    pub fn write_raw_i32(p: *const Chunk, idx: usize, val: i32);
    pub fn write_raw_i64(p: *const Chunk, idx: usize, val: i64);
    pub fn write_raw_f32(p: *const Chunk, idx: usize, val: f32);
    pub fn write_raw_f64(p: *const Chunk, idx: usize, val: f64);

    pub fn read_raw_i8 (p: *const Chunk, idx: usize) -> i8;
    pub fn read_raw_i16(p: *const Chunk, idx: usize) -> i16;
    pub fn read_raw_i32(p: *const Chunk, idx: usize) -> i32;
    pub fn read_raw_i64(p: *const Chunk, idx: usize) -> i64;
    pub fn read_raw_f32(p: *const Chunk, idx: usize) -> f32;
    pub fn read_raw_f64(p: *const Chunk, idx: usize) -> f64;
  }
}

#[cfg(test)]
mod tests {
  use types::{I32, F32, F64};

  use page::*;
  use page::c_api::*;

  #[test]
  fn test_page() {
    let p = Page::new(&[I32, F64], None);
    assert_eq!(2, p.chunk_num());

    unsafe {
      assert_eq!(p.chunk_ptr(0), get_chunk(&p, 0));
      assert_eq!(p.chunk_ptr(1), get_chunk(&p, 1));

      assert_eq!(&p.chunks()[0] as *const Chunk, get_chunk(&p, 0));
      assert_eq!(&p.chunks()[1] as *const Chunk, get_chunk(&p, 1));
    }

    assert_eq!(p.chunks().iter().map(|m| m.size).fold(0, |acc, s| acc + s), p.size());
    assert_eq!((4 + 8) * 1024, p.size());
  }

  /*
  #[test]
  fn test_chunk() {
    let p = Page::new(&[I32, F64], None);
  }
  */

  #[test]
  fn test_rw() {
    let p = Page::new(&[I32, F32, F64], None);

    let m = p.chunk_ptr(0);
    for x in 0..ROWBATCH_SIZE {
      unsafe { write_raw_i32(m, x, x as i32) };
    }
    for x in 0..ROWBATCH_SIZE {
      assert_eq!(x as i32, unsafe { read_raw_i32(m, x) });
    }

    let m = p.chunk_ptr(1);
    for x in 0..ROWBATCH_SIZE {
      unsafe { write_raw_f32(m, x, x as f32) };
    }
    for x in 0..ROWBATCH_SIZE {
      assert_eq!(x as f32, unsafe { read_raw_f32(m, x) });
    }

    let m = p.chunk_ptr(2);
    for x in 0..ROWBATCH_SIZE {
      unsafe { write_raw_f64(m, x, x as f64) };
    }
    for x in 0..ROWBATCH_SIZE {
      assert_eq!(x as f64, unsafe { read_raw_f64(m, x) });
    }
  }
}

/*
/// Type for column index
pub type PageId = usize;
/// Type for row position
pub type PosId = usize;

pub trait Page
{
	fn Chunk_num(&self) -> usize;

	fn set_value_count(&mut self, value_count: usize);

	fn value_count(&self) -> usize;

	fn Chunk(&self, id: PageId) -> &Chunk;

	fn bytesize(&self) -> u32;

	fn to_owned(&self) -> OwnedPage;

	fn project<'a>(&'a self, ids: &[PageId]) -> BorrowedPage<'a>
  {
  	let projected = ids.iter()
  		.map(|i| self.Chunk(*i))
  		.collect::<Vec<&Chunk>>();

    println!("projected: {}, vc: {}", projected.len(), self.value_count());

  	BorrowedPage::new(projected, self.value_count())
  }
}

pub struct OwnedPage
{
  mini_pages: Vec<Box<Chunk>>,

  value_count: usize
}

impl Page for OwnedPage
{
  #[inline]
  fn Chunk_num(&self) -> usize { self.mini_pages.len() }

  fn set_value_count(&mut self, value_count: usize) { self.value_count = value_count }

  #[inline]
  fn value_count(&self) -> usize { self.value_count }

  #[inline]
  fn Chunk(&self, id: PageId) -> &Chunk
  {
    debug_assert!(id < self.Chunk_num());

    &*self.mini_pages[id]
  }

  /// Total byte size of this page
  #[inline]
  fn bytesize(&self) -> u32
  {
    self.mini_pages.iter()
      .map(|m| m.bytesize())
      .fold(0, |acc, size| acc + size)
  }

  fn to_owned(&self) -> OwnedPage
  {
  	let copied_mpages = self.mini_pages
  		.iter()
  		.map(|mp| mp.copy())
  		.collect::<Vec<Box<Chunk>>>();

  	OwnedPage {mini_pages: copied_mpages, value_count: self.value_count}
  }
}

pub trait Chunk
{
  fn bytesize(&self) -> u32;

  fn read_i8(&self, pos: PosId) -> i8;

  fn read_i16(&self, pos: PosId) -> i16;

  fn read_i32(&self, pos: PosId) -> i32;

  fn read_i64(&self, pos: PosId) -> i64;

  fn read_f32(&self, pos: PosId) -> f32;

  fn read_f64(&self, pos: PosId) -> f64;

  fn as_i8_slice(&self) -> &[i8];

  fn as_i16_slice(&self) -> &[i16];

  fn as_i32_slice(&self) -> &[i32];

  fn as_i64_slice(&self) -> &[i64];

  fn as_f32_slice(&self) -> &[f32];

  fn as_f64_slice(&self) -> &[f64];

  fn writer(&mut self) -> &mut ChunkWriter;

  fn copy(&self) -> Box<Chunk>;
}

/// Writer for Vector. The writer internally must have a cursor to write a value.
/// For each write, the cursor must move forward the cursor.
/// You must call finalize() before reading any value from the Vector.
pub trait ChunkWriter
{
  fn write_i8(&mut self, v: i8);

  fn write_i16(&mut self, v: i16);

  fn write_i32(&mut self, v: i32);

  fn write_i64(&mut self, v: i64);

  fn write_f32(&mut self, v: f32);

  fn write_f64(&mut self, v: f64);

  fn write_bytes(&mut self, v: &[u8]);

  fn reset(&mut self);

  fn finalize(&mut self);
}

pub struct BorrowedPage<'a>
{
	borrowed: Vec<&'a Chunk>,
	value_count: usize
}

impl<'a> BorrowedPage<'a>
{
	#[inline]
	pub fn new(borrowed: Vec<&'a Chunk>, value_count: usize) -> BorrowedPage<'a>
	{
		BorrowedPage {
			borrowed   : borrowed,
			value_count: value_count
		}
	}
}

impl<'a> Page for BorrowedPage<'a>
{
	fn Chunk_num(&self) -> usize
	{
		self.borrowed.len()
	}

	fn set_value_count(&mut self, value_count: usize)
	{
		self.value_count = value_count;
	}

	fn value_count(&self) -> usize
	{
		self.value_count
	}

	fn Chunk(&self, id: PageId) -> &Chunk
	{
		self.borrowed[id]
	}

	fn bytesize(&self) -> u32
	{
		self.borrowed
			.iter()
			.map(|m| m.bytesize())
			.fold(0, |acc, size| acc + size)
	}

	fn to_owned(&self) -> OwnedPage
	{
		let copied_mpages = self.borrowed
  		.iter()
  		.map(|mp| mp.copy())
  		.collect::<Vec<Box<Chunk>>>();

  	OwnedPage {mini_pages: copied_mpages, value_count: self.value_count}
	}
}

pub struct OwnedPageBuilder
{
  page: OwnedPage
}

impl OwnedPageBuilder
{
  pub fn new(types: &Vec<Ty>) -> Self
  {
    let mini_pages = types
      .iter()
      .map(|ty| Box::new(FChunk::new(ty.size_of())) as Box<Chunk>)
      .collect::<Vec<Box<Chunk>>>();

    OwnedPageBuilder {page: OwnedPage {mini_pages: mini_pages, value_count: 0}}
  }

  #[inline]
  pub fn writer(&mut self, id: PageId) -> &mut ChunkWriter
  {
    self.page.mini_pages[id].writer()
  }

  #[inline]
  pub fn iter_mut<'a>(&'a mut self) -> Box<Iterator<Item=&'a mut ChunkWriter> + 'a> {
    Box::new(self.page.mini_pages.iter_mut().map(|m| m.writer()))
  }

  #[inline]
  pub fn reset(&mut self) {
    for v in self.page.mini_pages.iter_mut() {
      v.writer().reset();
    }
  }

  #[inline]
  pub fn build(&mut self, rownum: usize) -> &Page
  {
  	let mut value_count: usize = 0;
    for v in self.page.mini_pages.iter_mut() {
      v.writer().finalize()
    }

    self.page.set_value_count(rownum);
    &self.page
  }
}

/// Fixed Length Mini Page
pub struct FChunk<'a>
{
  ptr      : *mut u8,
  pub bytesize : u32,     // allocated memory size
  writer: FChunkWriter,

  _marker: marker::PhantomData<&'a ()>,
}

impl<'a> FChunk<'a>
{
  pub fn new(fixed_len: usize) -> FChunk<'a>
  {
    let required_size = get_aligned_size(fixed_len * ROWBATCH_SIZE);
    let ptr = unsafe { heap::allocate(required_size, 16) };

    FChunk {
      ptr: ptr,
      bytesize: required_size as u32,
      writer: FChunkWriter {ptr: ptr, len: required_size, pos: 0},
      _marker: marker::PhantomData,
    }
  }

  #[inline]
  pub fn as_ptr(&self) -> *const u8 {
    self.ptr
  }

  #[inline]
  pub fn as_mut_ptr(&mut self) -> *mut u8 {
    self.ptr
  }
}

impl<'a> Drop for FChunk<'a> {
  fn drop(&mut self) {
    unsafe {
      heap::deallocate(self.ptr as *mut u8, self.bytesize as usize, 16);
    }
  }
}

#[inline]
fn read_fixed_len_value<T>(ptr: *const u8, pos: PosId) -> T where T: Copy {
  debug_assert!(pos < ROWBATCH_SIZE);
  unsafe {
    let array: &[T] = slice::from_raw_parts(ptr as *const T, 1024);
    *array.get_unchecked(pos)
  }
}

macro_rules! read_field_for_fChunk(
	($name:ident, $ty:ty) => (
    #[inline]
	  fn $name(&self, pos: PosId) -> $ty {
    	read_fixed_len_value(self.ptr, pos)
    }
  );
);

macro_rules! as_slice_for_fChunk(
	($name:ident, $ty:ty) => (
    #[inline]
	  fn $name(&self) -> &[$ty] {
    	unsafe {
    		slice::from_raw_parts(self.ptr as *const $ty, 1024)
  		}
    }
  );
);

impl<'a> Chunk for FChunk<'a> {
  #[inline]
  fn bytesize(&self) -> u32 {
    self.bytesize
  }

  read_field_for_fChunk!(read_i8,  i8);
  read_field_for_fChunk!(read_i16, i16);
  read_field_for_fChunk!(read_i32, i32);
  read_field_for_fChunk!(read_i64, i64);
  read_field_for_fChunk!(read_f32, f32);
  read_field_for_fChunk!(read_f64, f64);

  as_slice_for_fChunk!(as_i8_slice,  i8);
  as_slice_for_fChunk!(as_i16_slice, i16);
  as_slice_for_fChunk!(as_i32_slice, i32);
  as_slice_for_fChunk!(as_i64_slice, i64);
  as_slice_for_fChunk!(as_f32_slice, f32);
  as_slice_for_fChunk!(as_f64_slice, f64);

  fn writer(&mut self) -> &mut ChunkWriter
  {
    &mut self.writer
  }

  fn copy(&self) -> Box<Chunk> {

  	let mut ptr = unsafe { heap::allocate(self.bytesize as usize, 16) };
    unsafe { ptr::copy_nonoverlapping(self.ptr, ptr, self.bytesize as usize); }

  	Box::new(FChunk {
      ptr: ptr,
      bytesize: self.bytesize as u32,
      writer: FChunkWriter {ptr: ptr, len: self.bytesize as usize, pos: 0},
      _marker: marker::PhantomData,
    })
  }
}


pub struct FChunkWriter {
  ptr: *mut u8,
  len: usize,
  pos: PosId
}

#[inline]
fn write_fixed_value<T>(ptr: *mut u8, pos: usize, val: T) {
  debug_assert!(pos < ROWBATCH_SIZE);
  unsafe {
    let array: &mut [T] = slice::from_raw_parts_mut(ptr as *mut T, ROWBATCH_SIZE);
    (*array.get_unchecked_mut(pos)) = val;
  }
}

macro_rules! write_value(
	($name:ident, $ty:ty) => (
    #[inline]
	  fn $name(&mut self, v: $ty) {
	    write_fixed_value(self.ptr, self.pos, v);
	    self.pos = self.pos + 1;
	  }
  );
);

impl ChunkWriter for FChunkWriter
{
	write_value!(write_i8, i8);
	write_value!(write_i16, i16);
	write_value!(write_i32, i32);
	write_value!(write_i64, i64);
	write_value!(write_f32, f32);
	write_value!(write_f64, f64);

  #[inline]
  fn write_bytes(&mut self, v: &[u8]) {
  }

  #[inline]
  fn reset(&mut self) {
    self.pos = 0;
  }

  #[inline]
  fn finalize(&mut self) {
  }
}

/// Borrowed Mini Page
///
/// It does not allocate its own memory.
/// Instead, just share the contents of other Chunk.
pub struct BorrowedChunk<'a>
{
  mini_page: &'a Chunk
}

macro_rules! read_field_for_bChunk(
	($name:ident, $ty:ty) => (
    #[inline]
	  fn $name(&self, pos: PosId) -> $ty {
    	self.mini_page.$name(pos)
    }
  );
);

macro_rules! as_slice_for_bChunk(
	($name:ident, $ty:ty) => (
    #[inline]
	  fn $name(&self) -> &[$ty] {
    	self.mini_page.$name()
    }
  );
);

impl<'a> Chunk for BorrowedChunk<'a> {
  #[inline]
  fn bytesize(&self) -> u32 {
    self.mini_page.bytesize()
  }

  read_field_for_bChunk!(read_i8, i8);
  read_field_for_bChunk!(read_i16, i16);
  read_field_for_bChunk!(read_i32, i32);
  read_field_for_bChunk!(read_i64, i64);
  read_field_for_bChunk!(read_f32, f32);
  read_field_for_bChunk!(read_f64, f64);

  as_slice_for_bChunk!(as_i8_slice, i8);
  as_slice_for_bChunk!(as_i16_slice, i16);
  as_slice_for_bChunk!(as_i32_slice, i32);
  as_slice_for_bChunk!(as_i64_slice, i64);
  as_slice_for_bChunk!(as_f32_slice, f32);
  as_slice_for_bChunk!(as_f64_slice, f64);

  fn writer(&mut self) -> &mut ChunkWriter
  {
    unreachable!("BorrowedChunk::writer() are not intended to be used");
  }

  fn copy(&self) -> Box<Chunk> {
  	self.mini_page.copy()
  }
}
*/