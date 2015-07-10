use types::{DataTy, HasDataTy};
use common::constant::VECTOR_SIZE;

use std::marker;
use std::marker::Sized;
use std::mem;
use std::slice;
use std::slice::Iter;
use std::raw::Slice;
use std::iter::Iterator;


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

pub struct ArrayVector<T: Sized> {
  array: Vec<T>,
  data_ty: DataTy
}

impl<'a, V> Vector for ArrayVector<V> {
  fn size(&self) -> usize { self.array.len() }
  
  #[inline]
  fn as_array<T>(&self) -> &[T] {
    unsafe {
      slice::from_raw_parts(self.array.as_ptr() as *const T, VECTOR_SIZE)
    }
  }

  #[inline]
  fn as_mut_array<T>(&mut self) -> &mut [T] {
    unsafe {
      slice::from_raw_parts_mut(self.array.as_mut_ptr() as *mut T, VECTOR_SIZE)
    }
  }

  // fn iter<T: 'a>(&self) -> Box<Iterator<Item=T>> {
  //   let slice = Slice {data: self.array.as_ptr() as *const T, len: self.array.len()};
  //   let x: &'a [T] = unsafe {
  //     mem::transmute(slice)
  //   };    

  //   Box::new(ArrayVectorItor {iter: x.iter()})
  // }
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

impl<T> HasDataTy for ArrayVector<T> {
  fn data_ty(&self) -> &DataTy {
    &self.data_ty
  }
}

/// Borrowed vector
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
}

impl<'a, 'b> Vector for PtrVector<'b> {
  fn size(&self) -> usize {self.size}

  fn as_array<T>(&self) -> &[T] {
    unsafe {
      slice::from_raw_parts(self.ptr as *const T, VECTOR_SIZE)
    }    
  }

  fn as_mut_array<T>(&mut self) -> &mut [T] {
    unsafe {
      slice::from_raw_parts_mut(self.ptr as *mut T, VECTOR_SIZE)
    }
  }
}

impl<'a> HasDataTy for PtrVector<'a> {
  fn data_ty(&self) -> &DataTy {
    &self.data_type
  }
}