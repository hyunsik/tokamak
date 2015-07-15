use alloc::heap;
use std::marker;
use std::mem;
use std::slice;
use std::slice::Iter;
use std::ptr;

use common::constant::VECTOR_SIZE;
use common::StringSlice;
use expr::Datum;
use intrinsics::sse;
use types::*;


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
  pub fn new(data_ty: DataTy) -> ArrayVector<'a> {
    let alloc_size = sse::compute_aligned_size(
      data_ty.bytes_len() as usize * VECTOR_SIZE);

    let ptr = unsafe {
      heap::allocate(alloc_size, sse::ALIGNED_SIZE)
    };

    ArrayVector {
      ptr: ptr,
      data_ty: data_ty,
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
  fn size(&self) -> usize { VECTOR_SIZE }

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

pub struct ConstVector {
  value: [u8; 16],
  data_ty: DataTy,
  datum: Datum
}

impl ConstVector {
  pub fn new(datum: Datum) -> ConstVector {
    let value: [u8; 16] = unsafe { mem::zeroed() };
    unsafe {
      match datum {
        Datum::Bool(v) => ptr::write(value.as_ptr() as *mut BOOL_T, v),
        Datum::Int1(v) => ptr::write(value.as_ptr() as *mut INT1_T, v),
        Datum::Int2(v) => ptr::write(value.as_ptr() as *mut INT2_T, v),
        Datum::Int4(v) => ptr::write(value.as_ptr() as *mut INT4_T, v),
        Datum::Int8(v) => ptr::write(value.as_ptr() as *mut INT8_T, v),
        Datum::Float4(v) => ptr::write(value.as_ptr() as *mut FLOAT4_T, v),
        Datum::Float8(v) => ptr::write(value.as_ptr() as *mut FLOAT8_T, v),
        Datum::Time(v) => ptr::write(value.as_ptr() as *mut TIME_T, v),        
        Datum::Date(v) => ptr::write(value.as_ptr() as *mut DATE_T, v),  
        Datum::Timestamp(v) => ptr::write(value.as_ptr() as *mut TIMESTAMP_T, v),  
        // Datum::Interval(v) => ptr::write(value.as_ptr() as *mut INTERVAL_T, v),  
        // Datum::Char(v) => ptr::write(value.as_ptr() , v),  
        Datum::Text(ref v) => {
          let text: TEXT_T = StringSlice::new_from_str(v.as_str());
          ptr::write(value.as_ptr() as *mut TEXT_T, text);
        }
        // Datum::Varchar(v) => ptr::write(value.as_ptr(), v),  
        // Datum::Blob(v) => ptr::write(value.as_ptr(), v),  
        _ => panic!("not support type")
      }
    }
    
    ConstVector {
      value: value,      
      data_ty: DataTy::new(datum.ty()),
      datum: datum
    }
  }
}

impl HasDataTy for ConstVector {
  fn data_ty(&self) -> &DataTy {
    &self.data_ty
  }
}

impl Vector for ConstVector {
  #[inline]
  fn size(&self) -> usize { 1 }

  #[inline]
  fn as_ptr(&self) -> *const u8 {
    self.value.as_ptr() as *const u8
  }

  #[inline]
  fn as_mut_ptr(&mut self) -> *mut u8 {
    self.value.as_ptr() as *mut u8
  }

  #[inline]
  fn is_const(&self) -> bool { false }
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

#[test]
fn test_const_vector() {
  let bool_vec: &Vector = &ConstVector::new(Datum::Bool(true));
  assert_eq!(Ty::Bool, bool_vec.data_ty().ty());
  assert_eq!(1, bool_vec.size());
  assert_eq!(true, *first_value(bool_vec));

  let int1_vec: &Vector = &ConstVector::new(Datum::Int1(7));
  assert_eq!(Ty::Int1, int1_vec.data_ty().ty());
  assert_eq!(1, int1_vec.size());
  assert_eq!(7, *first_value(int1_vec));

  let int2_vec: &Vector = &ConstVector::new(Datum::Int2(17));
  assert_eq!(Ty::Int2, int2_vec.data_ty().ty());
  assert_eq!(1, int2_vec.size());
  assert_eq!(17, *first_value(int2_vec));

  let int4_vec: &Vector = &ConstVector::new(Datum::Int4(178910));
  assert_eq!(Ty::Int4, int4_vec.data_ty().ty());
  assert_eq!(1, int4_vec.size());
  assert_eq!(178910, *first_value(int4_vec));

  let int8_vec: &Vector = &ConstVector::new(Datum::Int8(981627341));
  assert_eq!(Ty::Int8, int8_vec.data_ty().ty());
  assert_eq!(1, int8_vec.size());
  assert_eq!(981627341, *first_value(int8_vec));

  let float4_vec: &Vector = &ConstVector::new(Datum::Float4(3.14f32));
  assert_eq!(Ty::Float4, float4_vec.data_ty().ty());
  assert_eq!(1, float4_vec.size());
  assert_eq!(3.14f32, *first_value(float4_vec));

  let float8_vec: &Vector = &ConstVector::new(Datum::Float8(87123.1452f64));
  assert_eq!(Ty::Float8, float8_vec.data_ty().ty());
  assert_eq!(1, float8_vec.size());
  assert_eq!(87123.1452f64, *first_value(float8_vec));

  let text_vec: &Vector = &ConstVector::new(Datum::Text("hyunsik".to_string()));
  assert_eq!(Ty::Text, text_vec.data_ty().ty());
  assert_eq!(1, text_vec.size());
  let expected = StringSlice::new_from_str("hyunsik");
  assert_eq!(expected, *first_value(text_vec));
}