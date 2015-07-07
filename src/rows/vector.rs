use common::types::*;
use common::constant::VECTOR_SIZE;

use std::marker;
use std::mem;
use std::raw::Slice;
use std::iter::Iterator;


pub trait Vector1<'a> {
  fn data_type(&self) -> &DataType;
  fn size() -> usize;
  fn array<T>(&self) -> &[T];
  fn array_mut<T>(&self) -> &mut [T];
  fn iter<T>() -> Iterator<Item=T>;
}


pub struct Vector<'a> {
  ptr: *const u8,
  size: usize,
  data_type: DataType,
  _marker: marker::PhantomData<&'a ()>
}

impl<'a> Vector<'a> {
  pub fn new(ptr: *const u8, size: usize, data_type: DataType) -> Vector<'a> {
    Vector {
      ptr: ptr, 
      size: size,
      data_type: data_type, 
      _marker: marker::PhantomData
    }
  }

  pub fn data_type(&self) -> &DataType {
    &self.data_type
  }

  pub fn values_ptr(&self) -> *const u8 {
    self.ptr
  }  

  pub fn values<T>(&self) -> &mut [T] {
    let slice = Slice {data: self.ptr as *mut T, len: VECTOR_SIZE};
    unsafe {
      mem::transmute(slice)
    }
  }
}