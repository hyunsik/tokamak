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
  fn as_array<T>(&self) -> &[T];
  fn as_mut_array<T>(&mut self) -> &mut [T];
  //fn iter<T: 'a>(&self) -> Box<Iterator<Item=T>>;
}

pub trait VRowBlock<'b> {
  fn vector(&self, column_id: usize) -> &'b Vector;
  fn set_vector(&mut self, column_id: usize, &'b Vector);
}

pub struct BorrowVRowBlock<'b> {
  vectors: Vec<&'b Vector>
}

impl<'b> VRowBlock<'b> for BorrowVRowBlock<'b> {
  fn vector(&self, column_id: usize) -> &'b Vector {
    self.vectors[column_id]
  }

  fn set_vector(&mut self, column_id: usize, vector: &'b Vector) {
    self.vectors[column_id] = vector
  }
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
  fn as_array<T>(&self) -> &[T] {
    unsafe {
      slice::from_raw_parts(self.ptr as *const T, VECTOR_SIZE)
    }    
  }

  #[inline]
  fn as_mut_array<T>(&mut self) -> &mut [T] {
    unsafe {
      slice::from_raw_parts_mut(self.ptr as *mut T, VECTOR_SIZE)
    }
  }
}

// struct ArrayVectorItor<'a, T: 'a> {
//   iter: Iter<'a, T>
// }

// impl<'a, T: 'a> Iterator for ArrayVectorItor<'a, T> {
//   type Item = T;

//   fn next(&mut self) -> Option<T> {        
//     self.iter.next()
//   }
// }

impl<'a> HasDataTy for ArrayVector<'a> {
  fn data_ty(&self) -> &DataTy {
    &self.data_ty
  }
}