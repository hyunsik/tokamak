use types::{DataTy, HasDataTy};
use common::constant::VECTOR_SIZE;

use std::marker;
use std::mem;
use std::raw::Slice;
use std::iter::Iterator;


pub trait Vector<'a> : HasDataTy {
  fn size(&self) -> usize;
  unsafe fn as_array<T>(&self) -> &'a [T];
  unsafe fn as_mut_array<T>(&mut self) -> &'a mut [T];
  fn iter<T>() -> Iterator<Item=T>;
}

pub trait VRowBlock<'b> {
  fn vector(&self, column_id: usize) -> &'b Vector<'b>;
  fn set_vector(&mut self, column_id: usize, &'b Vector<'b>);
}

pub struct BorrowVRowBlock<'b> {
  vectors: Vec<&'b Vector<'b>>
}

impl<'b> VRowBlock<'b> for BorrowVRowBlock<'b> {
  fn vector(&self, column_id: usize) -> &'b Vector<'b> {
    self.vectors[column_id]
  }

  fn set_vector(&mut self, column_id: usize, vector: &'b Vector<'b>) {
    self.vectors[column_id] = vector
  }
}

pub struct ArrayVector<T> {
  array: Vec<T>,
  data_ty: DataTy
}

impl<'a, V> Vector<'a> for ArrayVector<V> {
  fn size(&self) -> usize { self.array.len() }
  
  unsafe fn as_array<T>(&self) -> &'a [T] {
    let slice = Slice {data: self.array.as_ptr() as *const T, len: self.array.len()};    
    mem::transmute(slice)        
  }

  unsafe fn as_mut_array<T>(&mut self) -> &'a mut [T] {
    let slice = Slice {data: self.array.as_mut_ptr() as *mut T, len: self.array.len()};
    unsafe {
      mem::transmute(slice)
    }
  }
}

impl<T> HasDataTy for ArrayVector<T> {
  fn data_ty(&self) -> &DataTy {
    &self.data_ty
  }
}


pub struct PtrVector<'a> {
  ptr: *const u8,
  size: usize,
  data_type: DataTy,
  _marker: marker::PhantomData<&'a ()>
}

impl<'a> PtrVector<'a> {
  pub fn new(ptr: *const u8, size: usize, data_type: DataTy) -> PtrVector<'a> {
    PtrVector {
      ptr: ptr, 
      size: size,
      data_type: data_type, 
      _marker: marker::PhantomData
    }
  }  

  pub fn as_array<T>(&self) -> &[T] {
    let slice = Slice {data: self.ptr as *const T, len: VECTOR_SIZE};
    unsafe {
      mem::transmute(slice)
    }
  }

  pub fn as_mut_array<T>(&self) -> &mut [T] {
    let slice = Slice {data: self.ptr as *mut T, len: VECTOR_SIZE};
    unsafe {
      mem::transmute(slice)
    }
  }
}

impl<'a> HasDataTy for PtrVector<'a> {
  fn data_ty(&self) -> &DataTy {
    &self.data_type
  }
}