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
use std::marker;
use std::ptr;
use std::slice;

use platform::get_aligned_size;
use types::Ty;

/// Each executor and operator process a batch of rows at a time for better throughput.
/// The experiment of MonetDB presented that 1024 is the best number of a row batch.
/// It's reason why I currently use 1024 as the number of row batch.
pub static ROWBATCH_SIZE: usize = 1024;

#[repr(C)]
pub enum MiniPageType {
  RAW = 0,
  RLE = 1
}

#[repr(C)]
pub struct MiniPage {
  pub ptr : *const u8,
  pub size: usize
}

impl MiniPage {
  pub fn new(elem_size: usize) -> MiniPage {
    let required_size = get_aligned_size(elem_size * ROWBATCH_SIZE);
    let ptr = unsafe { heap::allocate(required_size, 16) };

    MiniPage {
      ptr : ptr,
      size: required_size,
    }
  }
}

pub struct RawMiniPageWriter<'a> {
  mpage: &'a MiniPage,
  cur_idx: usize
}

#[repr(C)]
pub struct Page {
  pub mpages     : *const MiniPage,
  /// the number of minipages
  pub mpage_num  : usize,
  // the number of values stored in each minipage.
  // All minipages share the same val_cnt.
  pub value_cnt  : usize,
  // Does this page own the minipages?
  pub owned      : bool
}

impl Page {
  pub fn new(types: &[Ty]) -> Page {
    let mini_pages = types
      .iter()
      .map(|ty| MiniPage::new(ty.size_of()))
      .collect::<Vec<MiniPage>>();

    Page {
      mpages   : ::std::ptr::null(),
      mpage_num: types.len(),
      value_cnt: 0usize,
      owned    : true
    }
  }

  fn minipage(&self, page_id: usize) -> *const MiniPage {
    let ms: &[MiniPage] = unsafe { ::std::slice::from_raw_parts(self.mpages, self.mpage_num) };
    &ms[page_id]
  }

  fn minipage_num(&self) -> usize { self.mpage_num }

  fn set_value_count(&mut self, cnt: usize) { self.value_cnt = cnt }

  fn value_count(&self) -> usize { self.value_cnt}

  fn bytesize(&self) -> usize {
    let ms: &[MiniPage] = unsafe { ::std::slice::from_raw_parts(self.mpages, self.mpage_num) };
    ms.iter().map(|m| m.size).sum()
  }
}

pub mod c_api {
  use super::MiniPage;

  extern "C" {
    pub fn write_raw_i8 (p: *const MiniPage, idx: usize, val: i8);
    pub fn write_raw_i16(p: *const MiniPage, idx: usize, val: i16);
    pub fn write_raw_i32(p: *const MiniPage, idx: usize, val: i32);
    pub fn write_raw_i64(p: *const MiniPage, idx: usize, val: i64);
    pub fn write_raw_f32(p: *const MiniPage, idx: usize, val: f32);
    pub fn write_raw_f64(p: *const MiniPage, idx: usize, val: f64);

    pub fn read_raw_i8 (p: *const MiniPage, idx: usize) -> i8;
    pub fn read_raw_i16(p: *const MiniPage, idx: usize) -> i16;
    pub fn read_raw_i32(p: *const MiniPage, idx: usize) -> i32;
    pub fn read_raw_i64(p: *const MiniPage, idx: usize) -> i64;
    pub fn read_raw_f32(p: *const MiniPage, idx: usize) -> f32;
    pub fn read_raw_f64(p: *const MiniPage, idx: usize) -> f64;
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use rows::c_api::*;

  fn test_rows() {
    let m = MiniPage::new(4);
    unsafe { write_raw_i32(&m, 0, 32) };
    assert_eq!(32, unsafe { read_raw_i32(&m, 0) });
  }
}

/*
/// Type for column index
pub type PageId = usize;
/// Type for row position
pub type PosId = usize;

pub trait Page
{
	fn minipage_num(&self) -> usize;

	fn set_value_count(&mut self, value_count: usize);

	fn value_count(&self) -> usize;

	fn minipage(&self, id: PageId) -> &MiniPage;

	fn bytesize(&self) -> u32;

	fn to_owned(&self) -> OwnedPage;

	fn project<'a>(&'a self, ids: &[PageId]) -> BorrowedPage<'a>
  {
  	let projected = ids.iter()
  		.map(|i| self.minipage(*i))
  		.collect::<Vec<&MiniPage>>();

    println!("projected: {}, vc: {}", projected.len(), self.value_count());

  	BorrowedPage::new(projected, self.value_count())
  }
}

pub struct OwnedPage
{
  mini_pages: Vec<Box<MiniPage>>,

  value_count: usize
}

impl Page for OwnedPage
{
  #[inline]
  fn minipage_num(&self) -> usize { self.mini_pages.len() }

  fn set_value_count(&mut self, value_count: usize) { self.value_count = value_count }

  #[inline]
  fn value_count(&self) -> usize { self.value_count }

  #[inline]
  fn minipage(&self, id: PageId) -> &MiniPage
  {
    debug_assert!(id < self.minipage_num());

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
  		.collect::<Vec<Box<MiniPage>>>();

  	OwnedPage {mini_pages: copied_mpages, value_count: self.value_count}
  }
}

pub trait MiniPage
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

  fn writer(&mut self) -> &mut MiniPageWriter;

  fn copy(&self) -> Box<MiniPage>;
}

/// Writer for Vector. The writer internally must have a cursor to write a value.
/// For each write, the cursor must move forward the cursor.
/// You must call finalize() before reading any value from the Vector.
pub trait MiniPageWriter
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
	borrowed: Vec<&'a MiniPage>,
	value_count: usize
}

impl<'a> BorrowedPage<'a>
{
	#[inline]
	pub fn new(borrowed: Vec<&'a MiniPage>, value_count: usize) -> BorrowedPage<'a>
	{
		BorrowedPage {
			borrowed   : borrowed,
			value_count: value_count
		}
	}
}

impl<'a> Page for BorrowedPage<'a>
{
	fn minipage_num(&self) -> usize
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

	fn minipage(&self, id: PageId) -> &MiniPage
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
  		.collect::<Vec<Box<MiniPage>>>();

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
      .map(|ty| Box::new(FMiniPage::new(ty.size_of())) as Box<MiniPage>)
      .collect::<Vec<Box<MiniPage>>>();

    OwnedPageBuilder {page: OwnedPage {mini_pages: mini_pages, value_count: 0}}
  }

  #[inline]
  pub fn writer(&mut self, id: PageId) -> &mut MiniPageWriter
  {
    self.page.mini_pages[id].writer()
  }

  #[inline]
  pub fn iter_mut<'a>(&'a mut self) -> Box<Iterator<Item=&'a mut MiniPageWriter> + 'a> {
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
pub struct FMiniPage<'a>
{
  ptr      : *mut u8,
  pub bytesize : u32,     // allocated memory size
  writer: FMiniPageWriter,

  _marker: marker::PhantomData<&'a ()>,
}

impl<'a> FMiniPage<'a>
{
  pub fn new(fixed_len: usize) -> FMiniPage<'a>
  {
    let required_size = get_aligned_size(fixed_len * ROWBATCH_SIZE);
    let ptr = unsafe { heap::allocate(required_size, 16) };

    FMiniPage {
      ptr: ptr,
      bytesize: required_size as u32,
      writer: FMiniPageWriter {ptr: ptr, len: required_size, pos: 0},
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

impl<'a> Drop for FMiniPage<'a> {
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

macro_rules! read_field_for_fminipage(
	($name:ident, $ty:ty) => (
    #[inline]
	  fn $name(&self, pos: PosId) -> $ty {
    	read_fixed_len_value(self.ptr, pos)
    }
  );
);

macro_rules! as_slice_for_fminipage(
	($name:ident, $ty:ty) => (
    #[inline]
	  fn $name(&self) -> &[$ty] {
    	unsafe {
    		slice::from_raw_parts(self.ptr as *const $ty, 1024)
  		}
    }
  );
);

impl<'a> MiniPage for FMiniPage<'a> {
  #[inline]
  fn bytesize(&self) -> u32 {
    self.bytesize
  }

  read_field_for_fminipage!(read_i8,  i8);
  read_field_for_fminipage!(read_i16, i16);
  read_field_for_fminipage!(read_i32, i32);
  read_field_for_fminipage!(read_i64, i64);
  read_field_for_fminipage!(read_f32, f32);
  read_field_for_fminipage!(read_f64, f64);

  as_slice_for_fminipage!(as_i8_slice,  i8);
  as_slice_for_fminipage!(as_i16_slice, i16);
  as_slice_for_fminipage!(as_i32_slice, i32);
  as_slice_for_fminipage!(as_i64_slice, i64);
  as_slice_for_fminipage!(as_f32_slice, f32);
  as_slice_for_fminipage!(as_f64_slice, f64);

  fn writer(&mut self) -> &mut MiniPageWriter
  {
    &mut self.writer
  }

  fn copy(&self) -> Box<MiniPage> {

  	let mut ptr = unsafe { heap::allocate(self.bytesize as usize, 16) };
    unsafe { ptr::copy_nonoverlapping(self.ptr, ptr, self.bytesize as usize); }

  	Box::new(FMiniPage {
      ptr: ptr,
      bytesize: self.bytesize as u32,
      writer: FMiniPageWriter {ptr: ptr, len: self.bytesize as usize, pos: 0},
      _marker: marker::PhantomData,
    })
  }
}


pub struct FMiniPageWriter {
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

impl MiniPageWriter for FMiniPageWriter
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
/// Instead, just share the contents of other minipage.
pub struct BorrowedMiniPage<'a>
{
  mini_page: &'a MiniPage
}

macro_rules! read_field_for_bminipage(
	($name:ident, $ty:ty) => (
    #[inline]
	  fn $name(&self, pos: PosId) -> $ty {
    	self.mini_page.$name(pos)
    }
  );
);

macro_rules! as_slice_for_bminipage(
	($name:ident, $ty:ty) => (
    #[inline]
	  fn $name(&self) -> &[$ty] {
    	self.mini_page.$name()
    }
  );
);

impl<'a> MiniPage for BorrowedMiniPage<'a> {
  #[inline]
  fn bytesize(&self) -> u32 {
    self.mini_page.bytesize()
  }

  read_field_for_bminipage!(read_i8, i8);
  read_field_for_bminipage!(read_i16, i16);
  read_field_for_bminipage!(read_i32, i32);
  read_field_for_bminipage!(read_i64, i64);
  read_field_for_bminipage!(read_f32, f32);
  read_field_for_bminipage!(read_f64, f64);

  as_slice_for_bminipage!(as_i8_slice, i8);
  as_slice_for_bminipage!(as_i16_slice, i16);
  as_slice_for_bminipage!(as_i32_slice, i32);
  as_slice_for_bminipage!(as_i64_slice, i64);
  as_slice_for_bminipage!(as_f32_slice, f32);
  as_slice_for_bminipage!(as_f64_slice, f64);

  fn writer(&mut self) -> &mut MiniPageWriter
  {
    unreachable!("BorrowedMiniPage::writer() are not intended to be used");
  }

  fn copy(&self) -> Box<MiniPage> {
  	self.mini_page.copy()
  }
}
*/