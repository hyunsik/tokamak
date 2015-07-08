use types::{DataTy, HasDataTy};
use common::constant::VECTOR_SIZE;

use std::marker;
use std::mem;
use std::raw::Slice;
use std::iter::Iterator;


pub trait Vector1 : HasDataTy {
  fn size(&self) -> usize;
  fn array<T>(&self) -> &[T];
  //fn array_mut<T>(&self) -> &mut [T];
  //fn iter<T>() -> Iterator<Item=T>;
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

impl<V> Vector1 for ArrayVector<V> {
  fn size(&self) -> usize { self.array.len() }
  fn array<T>(&self) -> &[T] { 
    unsafe {
      mem::transmute(&*self.array)    
    }    
  }
}

impl<T> HasDataTy for ArrayVector<T> {
  fn data_ty(&self) -> &DataTy {
    &self.data_ty
  }
}


pub struct Vector<'a> {
  ptr: *const u8,
  size: usize,
  data_type: DataTy,
  _marker: marker::PhantomData<&'a ()>
}

impl<'a> Vector<'a> {
  pub fn new(ptr: *const u8, size: usize, data_type: DataTy) -> Vector<'a> {
    Vector {
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

impl<'a> HasDataTy for Vector<'a> {
  fn data_ty(&self) -> &DataTy {
    &self.data_type
  }
}