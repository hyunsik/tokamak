use alloc::heap;
use std::marker;

use common::constant::VECTOR_SIZE;
use intrinsics::sse;
use types::*;
use std::slice;

trait Vector {
  fn size(&self) -> usize;
  fn as_ptr(&self) -> *const u8;
  fn as_mut_ptr(&mut self) ->*mut u8;
}

struct ArrayVector {
  ptr: *mut u8,
  data_ty: DataTy
}

impl ArrayVector {
  pub fn new(data_ty: &DataTy) -> ArrayVector {
    let alloc_size = sse::compute_aligned_size(
      data_ty.bytes_len() as usize * VECTOR_SIZE);

    let ptr = unsafe {
      heap::allocate(alloc_size, sse::ALIGNED_SIZE)
    };

    ArrayVector {
      ptr: ptr,
      data_ty: data_ty.clone(),
    }
  }
}

impl Drop for ArrayVector {
  fn drop(&mut self) {
    unsafe {
      let alloc_size = sse::compute_aligned_size(
        self.data_ty.bytes_len() as usize * VECTOR_SIZE);

      heap::deallocate(self.ptr as *mut u8, alloc_size, sse::ALIGNED_SIZE);
    }
  }
}

impl Vector for ArrayVector {
  #[inline]
  fn size(&self) -> usize {
    VECTOR_SIZE
  } 

  #[inline]
  fn as_ptr(&self) -> *const u8 {
    self.ptr
  }

  #[inline]
  fn as_mut_ptr(&mut self) -> *mut u8 {
    self.ptr
  }
}

#[inline]
fn as_array<T>(v: &Vector) -> &[T] {
  unsafe {
    slice::from_raw_parts(v.as_ptr() as *const T, VECTOR_SIZE)
  }    
}

#[inline]
fn as_mut_array<T>(v: &mut Vector) -> &[T] {
  unsafe {
    slice::from_raw_parts(v.as_mut_ptr() as *mut T, VECTOR_SIZE)
  }    
}

trait RowBlock<'b> {
  fn vector(&'b self, cid: usize) -> &'b Vector;
}

struct AssemblyRowBlock<'b> {
  vectors: Vec<Box<&'b Vector>>
}

impl<'b> AssemblyRowBlock<'b> {
  fn set_vector(&mut self, v: &'b Vector) {
    self.vectors.push(Box::new(v));
  }
}

impl<'b> RowBlock<'b> for AssemblyRowBlock<'b> {
 fn vector(&self, cid: usize) -> &'b Vector {
   *self.vectors[cid]
 } 
}

struct YRowBlock<'a> {
  vectors: Vec<Box<Vector>>,
  _marker: marker::PhantomData<&'a ()>
}

impl<'a> YRowBlock<'a> {
  fn set_vector(&mut self, v: Box<Vector>) {
    self.vectors.push(v);
  }
}

impl<'a> RowBlock<'a> for YRowBlock<'a> {
 fn vector(&'a self, cid: usize) -> &'a Vector {   
   &*self.vectors[cid]
 }
}

#[test]
fn test_yrowblock() {
  let mut y = YRowBlock {vectors: Vec::new(), _marker: marker::PhantomData};
  let vector = ArrayVector::new(&DataTy::new(Ty::Int4));  
  y.set_vector(Box::new(vector));

  let mut ass = AssemblyRowBlock {vectors: Vec::new()};
  ass.set_vector(y.vector(0));
  let v: &Vector = ass.vector(0);
}