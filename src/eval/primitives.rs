use std::ops;

use common::constant::VECTOR_SIZE;
use rows::vector;
use rows::vector::{Vector, as_mut_array, as_array};

#[inline]
pub fn map_plus_vv<T: ops::Add>(res: &mut Vector, lhs: &Vector, rhs: &Vector) 
                                where T : Copy + ops::Add<T, Output=T> {

  let t: &mut [T] = as_mut_array(res);
  let l: &[T] = as_array(lhs);
  let r: &[T] = as_array(rhs);

  unsafe {
    for i in 0..VECTOR_SIZE {
      *t.get_unchecked_mut(i) = *l.get_unchecked(i) + *r.get_unchecked(i);
    }
  }
}

#[inline]
pub fn map_plus_cv<T: ops::Add>(res: &mut Vector, lhs: &Vector, rhs: &Vector) 
                                where T : Copy + ops::Add<T, Output=T> {

  let t: &mut [T] = as_mut_array(res);
  let l: T = *vector::first_value(lhs);
  let r: &[T] = as_array(rhs);  

  unsafe {
    for i in 0..VECTOR_SIZE {
      *t.get_unchecked_mut(i) = l + *r.get_unchecked(i);
    }
  }
}

#[inline]
pub fn map_plus_vc<T: ops::Add>(res: &mut Vector, lhs: &Vector, rhs: &Vector) 
                                where T : Copy + ops::Add<T, Output=T> {

  let t: &mut [T] = as_mut_array(res);
  let l: &[T] = as_array(lhs);
  let r: T = *vector::first_value(rhs);  

  unsafe {
    for i in 0..VECTOR_SIZE {
      *t.get_unchecked_mut(i) = *l.get_unchecked(i) + r;
    }
  }
}