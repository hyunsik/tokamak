use alloc::heap;
use std::marker;
use std::marker::Sized;
use std::mem;
use std::slice;
use std::slice::Iter;
use std::raw::Slice;
use std::iter::Iterator;

use intrinsics::sse;
use types::{DataTy, HasDataTy};
use common::constant::VECTOR_SIZE;


pub trait Vector : HasDataTy {
  fn size(&self) -> usize;
  fn as_ptr(&self) -> *const u8;
  fn as_mut_ptr(&mut self) ->*mut u8;
  fn is_const(&self) -> bool;
}

pub struct ArrayVector<'a> {
  ptr: *mut u8,
  data_ty: DataTy,
  _marker: marker::PhantomData<&'a ()>
}

impl<'a> ArrayVector<'a> {
  pub fn new(data_ty: &DataTy) -> ArrayVector {
    let alloc_size = sse::compute_aligned_size(
      data_ty.bytes_len() as usize * VECTOR_SIZE);

    let ptr = unsafe {
      heap::allocate(alloc_size, sse::ALIGNED_SIZE)
    };

    ArrayVector {
      ptr: ptr,
      data_ty: data_ty.clone(),
      _marker: marker::PhantomData
    }
  }
}

impl<'a> Drop for ArrayVector<'a> {
  fn drop(&mut self) {
    unsafe {
      let alloc_size = sse::compute_aligned_size(
        self.data_ty.bytes_len() as usize * VECTOR_SIZE);

      heap::deallocate(self.ptr as *mut u8, alloc_size, sse::ALIGNED_SIZE);
    }
  }
}

impl<'a> Vector for ArrayVector<'a> {
  #[inline]
  fn size(&self) -> usize {VECTOR_SIZE}

  #[inline]
  fn as_ptr(&self) -> *const u8 {
    self.ptr
  }

  #[inline]
  fn as_mut_ptr(&mut self) -> *mut u8 {
    self.ptr
  }

  #[inline]
  fn is_const(&self) -> bool { false }
}

impl<'a> HasDataTy for ArrayVector<'a> {
  fn data_ty(&self) -> &DataTy {
    &self.data_ty
  }
}

#[inline]
pub fn first_value<T>(v: &Vector) -> &T {
  let array = unsafe {
    slice::from_raw_parts(v.as_ptr() as *const T, 1) as &[T]
  };

  &array[0]
}

#[inline]
pub fn as_array<T>(v: &Vector) -> &[T] {
  unsafe {
    slice::from_raw_parts(v.as_ptr() as *const T, VECTOR_SIZE)
  }    
}

#[inline]
pub fn as_mut_array<T>(v: &mut Vector) -> &mut [T] {
  unsafe {
    slice::from_raw_parts_mut(v.as_mut_ptr() as *mut T, VECTOR_SIZE)
  }    
}