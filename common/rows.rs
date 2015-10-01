/// This row implementation is basically based on Pax, 
/// but it has a variation in terms of variable-length blocks.

use alloc::heap;
use std::marker;

use types::Type;

pub static ALIGNED_SIZE : usize = 16;
pub static ROWBATCH_SIZE: usize = 1024; 

pub type ColumnId = u16;

pub fn compute_aligned_size(size: usize) -> usize { 
  let remain = size % ALIGNED_SIZE;
  
  if remain > 0 {
    size + (ALIGNED_SIZE - remain)
  } else {
    size
  }
}  

pub trait PageBuilder {
  fn get_Vector(&self, cid: ColumnId) -> &Vector;
  fn build(&mut self) -> &mut Page;
}

pub struct DefaultPageBuilder {
  page : Page   
}

impl DefaultPageBuilder 
{
  pub fn new(types: &Vec<Box<Type>>) -> DefaultPageBuilder {
    
    let mini_pages = types
      .iter()
      .map(|ty| {
        ty.create_vector()
      })
      .collect::<Vec<Box<Vector>>>();
    
    DefaultPageBuilder {
      page: Page { mini_pages: mini_pages }
    }
  }
}

impl PageBuilder for DefaultPageBuilder {
  fn get_Vector(&self, cid: ColumnId) -> &Vector {
    &*self.page.mini_pages[cid as usize]
  }
  
  fn build(&mut self) -> &mut Page {
    &mut self.page    
  }
}

pub struct Page {
  mini_pages: Vec<Box<Vector>> 
}

impl Page {
  fn column_num(&self) -> u32 { self.mini_pages.len() as u32 }
}

pub trait Vector {
  fn size_in_bytes(&self) -> u32;
}

pub trait VectorBuilder {
  fn write_i8(&mut self, v: i8);
  
  fn write_i16(&mut self, v: i16);
  
  fn write_i32(&mut self, v: i32);
  
  fn write_i64(&mut self, v: i64);
  
  fn write_f32(&mut self, v: f32);
  
  fn write_f64(&mut self, v: f64);
  
  fn write_bytes(&mut self, v: &[u8]);
  
  fn reset(&mut self);
  
  fn build(&mut self) -> &Vector;
}

pub struct FixedLenVector<'a> {
  ptr: *mut u8,
  size: u32,
  _marker: marker::PhantomData<&'a ()>  
}

impl<'a> FixedLenVector<'a> {
  pub fn new(fixed_len: usize) -> FixedLenVector<'a> {
    let alloc_size = compute_aligned_size(fixed_len * ROWBATCH_SIZE);

    let ptr = unsafe { heap::allocate(alloc_size, 16) };

    FixedLenVector {
      ptr: ptr,
      size: alloc_size as u32,
      _marker: marker::PhantomData
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

impl<'a> Drop for FixedLenVector<'a> {
  fn drop(&mut self) {
    unsafe {
      heap::deallocate(self.ptr as *mut u8, self.size as usize, 16);
    }
  }
}

impl<'a> Vector for FixedLenVector<'a> {
  fn size_in_bytes(&self) -> u32 {
    self.size
  }
}

pub struct FixedLenVectorBuilder<'a> {
  fixed_len: usize,
  pos : usize,
  vector: FixedLenVector<'a>    
}

impl<'a> FixedLenVectorBuilder<'a> 
{
  fn new(fixed_len: usize) -> FixedLenVectorBuilder<'a> {
    
    FixedLenVectorBuilder {
      fixed_len: fixed_len,
      pos: 0,
      vector: FixedLenVector::new(fixed_len)
    }
  }
}

use std::slice;

#[inline]
fn as_array<'a, T>(v: &FixedLenVector<'a>) -> &'a [T] {
  unsafe {
    slice::from_raw_parts(v.as_ptr() as *const T, 1024)
  }
}

#[inline]
pub fn as_mut_array<'a, T>(v: &mut FixedLenVector<'a>) -> &'a mut [T] {
  unsafe {
    slice::from_raw_parts_mut(v.as_mut_ptr() as *mut T, 1024)
  }
}

#[inline]
fn write_fixed_value<T>(vec: &mut FixedLenVector, pos: usize, val: T) {
  unsafe {
    let array: &mut [T] = slice::from_raw_parts_mut(vec.as_mut_ptr() as *mut T, 1024);
    (*array.get_unchecked_mut(pos)) = val;
  }
}

impl<'a> VectorBuilder for FixedLenVectorBuilder<'a> 
{
  #[inline]
  fn write_i8(&mut self, v: i8) {
    write_fixed_value(&mut self.vector, self.pos, v);
    self.pos = self.pos + 1;
  }
  
  #[inline]
  fn write_i16(&mut self, v: i16) {
    write_fixed_value(&mut self.vector, self.pos, v);
    self.pos = self.pos + 1;
  }
  
  #[inline]
  fn write_i32(&mut self, v: i32) {
    write_fixed_value(&mut self.vector, self.pos, v);
    self.pos = self.pos + 1;
  }
  
  #[inline]
  fn write_i64(&mut self, v: i64) {
    write_fixed_value(&mut self.vector, self.pos, v);
    self.pos = self.pos + 1;
  }
  
  #[inline]
  fn write_f32(&mut self, v: f32) {
    write_fixed_value(&mut self.vector, self.pos, v);
    self.pos = self.pos + 1;
  }

  #[inline]  
  fn write_f64(&mut self, v: f64) {
    write_fixed_value(&mut self.vector, self.pos, v);
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
  fn build(&mut self) -> &Vector {
    &self.vector
  }
}