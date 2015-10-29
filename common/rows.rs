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
use std::slice;

use platform::get_aligned_size;
use types::Ty;

/// Each executor and operator process a batch of rows at a time for better throughput.
/// The experiment of MonetDB presented that 1024 is the best number of a row batch.
/// It's reason why I currently use 1024 as the number of row batch. 
pub static ROWBATCH_SIZE: usize = 1024; 

/// Type for column index
pub type PageId = usize;
/// Type for row position
pub type PosId = usize;

pub struct Page 
{
  mini_pages: Vec<Box<MiniPage>>,
  
  value_count: u32
}

impl Page 
{
  #[inline]
  pub fn minipage_num(&self) -> usize { self.mini_pages.len() }
  
  #[inline]
  pub fn value_count(&self) -> u32 { self.value_count }
  
  #[inline]
  pub fn minipage(&self, id: PageId) -> &MiniPage 
  {
    debug_assert!(id < self.minipage_num());
     
    &*self.mini_pages[id] 
  }
  
  /// Total byte size of this page
  #[inline]
  pub fn bytesize(&self) -> u32 
  {
    self.mini_pages.iter()
      .map(|m| m.bytesize())
      .fold(0, |acc, size| acc + size)
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
  
  fn writer(&mut self) -> &mut MiniPageWriter;
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


pub struct PageBuilder 
{
  page: Page
}

impl PageBuilder 
{
  pub fn new(types: &Vec<Ty>) -> Self 
  {
    let mini_pages = types
      .iter()
      .map(|ty| ty.handler())
      .map(|f| (f.create_minipage)())
      .collect::<Vec<Box<MiniPage>>>();
    
    PageBuilder {page: Page {mini_pages: mini_pages, value_count: 0}}
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
  pub fn build(&mut self) -> &Page 
  {
    for v in self.page.mini_pages.iter_mut() {
      v.writer().finalize();
    }
    
    &self.page
  }
}

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

impl<'a> MiniPage for FMiniPage<'a> {
  #[inline]
  fn bytesize(&self) -> u32 {
    self.bytesize
  }
  
  fn read_i8(&self, pos: PosId) -> i8 {
    read_fixed_len_value(self.ptr, pos)
  }
  
  fn read_i16(&self, pos: PosId) -> i16 {
    read_fixed_len_value(self.ptr, pos)
  }
  
  fn read_i32(&self, pos: PosId) -> i32 {
    read_fixed_len_value(self.ptr, pos)
  }
  
  fn read_i64(&self, pos: PosId) -> i64 {
    read_fixed_len_value(self.ptr, pos)
  }
  
  fn read_f32(&self, pos: PosId) -> f32 {
    read_fixed_len_value(self.ptr, pos)
  }
  
  fn read_f64(&self, pos: PosId) -> f64 {
    read_fixed_len_value(self.ptr, pos)
  }
  
  fn writer(&mut self) -> &mut MiniPageWriter 
  {
    &mut self.writer
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

impl MiniPageWriter for FMiniPageWriter 
{
  #[inline]
  fn write_i8(&mut self, v: i8) {
    write_fixed_value(self.ptr, self.pos, v);
    self.pos = self.pos + 1;
  }
  
  #[inline]
  fn write_i16(&mut self, v: i16) {
    write_fixed_value(self.ptr, self.pos, v);
    self.pos = self.pos + 1;
  }
  
  #[inline]
  fn write_i32(&mut self, v: i32) {
    write_fixed_value(self.ptr, self.pos, v);
    self.pos = self.pos + 1;
  }
  
  #[inline]
  fn write_i64(&mut self, v: i64) {
    write_fixed_value(self.ptr, self.pos, v);
    self.pos = self.pos + 1;
  }
  
  #[inline]
  fn write_f32(&mut self, v: f32) {
    write_fixed_value(self.ptr, self.pos, v);
    self.pos = self.pos + 1;
  }

  #[inline]  
  fn write_f64(&mut self, v: f64) {
    write_fixed_value(self.ptr, self.pos, v);
    self.pos = self.pos + 1;
  }
  
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