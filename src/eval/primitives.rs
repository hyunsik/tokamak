//!
//! Vectorized Expression Evaluator Primitives
//!
use std::ops;
use std::fmt::Display;

use common::constant::VECTOR_SIZE;
use rows::vector;
use rows::vector::{Vector, as_mut_array, as_array};

pub fn map_plus_vv<T: ops::Add>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                                selected: Option<&[usize]>) 
                                where T : Copy + Display + ops::Add<T, Output=T> {

  let t: &mut [T] = as_mut_array(res);
  let l: &[T] = as_array(lhs);
  let r: &[T] = as_array(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = *l.get_unchecked(sel_id) + *r.get_unchecked(sel_id);
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        let ld:T = *l.get_unchecked(i);
        let rd:T = *r.get_unchecked(i);
        let plusd = ld + rd;
        println!("{}: {} = {} + {}", i, plusd, ld, rd);
        *t.get_unchecked_mut(i) = *l.get_unchecked(i) + *r.get_unchecked(i);
      }
    }
  }
}