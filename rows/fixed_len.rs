/// FMiniPage is a MiniPage implementation for fixed-length values.
/// FMiniPage uses a fixed-width dense array, so it is good for cache hits 
/// and allows SIMD operations.

use alloc::heap;
use std::marker;
use std::slice;
use std::rc::Rc;
use std::cell::RefCell;

use common::platform::get_aligned_size;

use common::rows::{
  MiniPage,
  MiniPageWriter,
  PageId,
  PosId,
  ROWBATCH_SIZE
};

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