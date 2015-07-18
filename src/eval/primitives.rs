//!
//! Vectorized Expression Evaluator Primitives
//!
use std::ops;
use std::fmt::Display;

use common::constant::VECTOR_SIZE;
use rows::vector;
use rows::vector::{as_mut_array, as_array, first_value, Vector};
use types::BOOL_T;

// Map::And ------------------------------------------------------------------
pub fn map_and_vv(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                                selected: Option<&[usize]>) {
  let t: &mut [BOOL_T] = as_mut_array(res);
  let l: &[BOOL_T] = as_array(lhs);
  let r: &[BOOL_T] = as_array(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = *l.get_unchecked(sel_id) && *r.get_unchecked(sel_id);
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = *l.get_unchecked(i) && *r.get_unchecked(i);
      }
    }
  }
}

pub fn map_and_vc(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                                selected: Option<&[usize]>) {
  let t: &mut [BOOL_T] = as_mut_array(res);
  let l: &[BOOL_T] = as_array(lhs);
  let r: BOOL_T = *first_value(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = *l.get_unchecked(sel_id) && r;
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = *l.get_unchecked(i) && r;
      }
    }
  }
}

pub fn map_and_cv(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                                selected: Option<&[usize]>) {
  let t: &mut [BOOL_T] = as_mut_array(res);
  let l: BOOL_T = *first_value(lhs);
  let r: &[BOOL_T] = as_array(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = l && *r.get_unchecked(sel_id)
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = l && *r.get_unchecked(i);
      }
    }
  }
}

// Map::Comp::Eq -------------------------------------------------------------
pub fn map_eq_vv<T>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                                selected: Option<&[usize]>) 
                                where T : Copy + Display + PartialEq {
  let t: &mut [bool] = as_mut_array(res);
  let l: &[T] = as_array(lhs);
  let r: &[T] = as_array(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = *l.get_unchecked(sel_id) == *r.get_unchecked(sel_id);
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = *l.get_unchecked(i) == *r.get_unchecked(i);
      }
    }
  }
}

pub fn map_eq_vc<T>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                                selected: Option<&[usize]>) 
                                where T : Copy + Display + PartialEq {

  let t: &mut [bool] = as_mut_array(res);
  let l: &[T] = as_array(lhs);
  let r: T = *first_value(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = *l.get_unchecked(sel_id) == r;
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = *l.get_unchecked(i) == r;
      }
    }
  }
}

pub fn map_eq_cv<T>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                      selected: Option<&[usize]>) 
                      where T : Copy + Display + PartialEq {

  let t: &mut [bool] = as_mut_array(res);
  let l: T = *first_value(lhs);
  let r: &[T] = as_array(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = l == *r.get_unchecked(sel_id);
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = l == *r.get_unchecked(i);
      }
    }
  }
}

// Map::Comp::Ne -------------------------------------------------------------
pub fn map_ne_vv<T>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                                selected: Option<&[usize]>) 
                                where T : Copy + Display + PartialEq {
  let t: &mut [bool] = as_mut_array(res);
  let l: &[T] = as_array(lhs);
  let r: &[T] = as_array(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = *l.get_unchecked(sel_id) != *r.get_unchecked(sel_id);
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = *l.get_unchecked(i) != *r.get_unchecked(i);
      }
    }
  }
}

pub fn map_ne_vc<T>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                                selected: Option<&[usize]>) 
                                where T : Copy + Display + PartialEq {

  let t: &mut [bool] = as_mut_array(res);
  let l: &[T] = as_array(lhs);
  let r: T = *first_value(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = *l.get_unchecked(sel_id) != r;
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = *l.get_unchecked(i) != r;
      }
    }
  }
}

pub fn map_ne_cv<T>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                      selected: Option<&[usize]>) 
                      where T : Copy + Display + PartialEq {

  let t: &mut [bool] = as_mut_array(res);
  let l: T = *first_value(lhs);
  let r: &[T] = as_array(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = l != *r.get_unchecked(sel_id);
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = l != *r.get_unchecked(i);
      }
    }
  }
}

// Map::Comp::Lt -------------------------------------------------------------
pub fn map_lt_vv<T>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                                selected: Option<&[usize]>) 
                                where T : Copy + Display + PartialOrd {
  let t: &mut [bool] = as_mut_array(res);
  let l: &[T] = as_array(lhs);
  let r: &[T] = as_array(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = *l.get_unchecked(sel_id) < *r.get_unchecked(sel_id);
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = *l.get_unchecked(i) < *r.get_unchecked(i);
      }
    }
  }
}

pub fn map_lt_vc<T>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                                selected: Option<&[usize]>) 
                                where T : Copy + Display + PartialOrd {

  let t: &mut [bool] = as_mut_array(res);
  let l: &[T] = as_array(lhs);
  let r: T = *first_value(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = *l.get_unchecked(sel_id) < r;
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = *l.get_unchecked(i) < r;
      }
    }
  }
}

pub fn map_lt_cv<T>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                      selected: Option<&[usize]>) 
                      where T : Copy + Display + PartialOrd {

  let t: &mut [bool] = as_mut_array(res);
  let l: T = *first_value(lhs);
  let r: &[T] = as_array(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = l < *r.get_unchecked(sel_id);
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = l < *r.get_unchecked(i);
      }
    }
  }
}

// Map::Comp::Le -------------------------------------------------------------
pub fn map_le_vv<T>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                                selected: Option<&[usize]>) 
                                where T : Copy + Display + PartialOrd {
  let t: &mut [bool] = as_mut_array(res);
  let l: &[T] = as_array(lhs);
  let r: &[T] = as_array(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = *l.get_unchecked(sel_id) <= *r.get_unchecked(sel_id);
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = *l.get_unchecked(i) <= *r.get_unchecked(i);
      }
    }
  }
}

pub fn map_le_vc<T>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                                selected: Option<&[usize]>) 
                                where T : Copy + Display + PartialOrd {

  let t: &mut [bool] = as_mut_array(res);
  let l: &[T] = as_array(lhs);
  let r: T = *first_value(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = *l.get_unchecked(sel_id) <= r;
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = *l.get_unchecked(i) <= r;
      }
    }
  }
}

pub fn map_le_cv<T>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                      selected: Option<&[usize]>) 
                      where T : Copy + Display + PartialOrd {

  let t: &mut [bool] = as_mut_array(res);
  let l: T = *first_value(lhs);
  let r: &[T] = as_array(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = l <= *r.get_unchecked(sel_id);
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = l <= *r.get_unchecked(i);
      }
    }
  }
}


// Map::Comp::Gt -------------------------------------------------------------
pub fn map_gt_vv<T>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                                selected: Option<&[usize]>) 
                                where T : Copy + Display + PartialOrd {
  let t: &mut [bool] = as_mut_array(res);
  let l: &[T] = as_array(lhs);
  let r: &[T] = as_array(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = *l.get_unchecked(sel_id) > *r.get_unchecked(sel_id);
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = *l.get_unchecked(i) > *r.get_unchecked(i);
      }
    }
  }
}

pub fn map_gt_vc<T>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                                selected: Option<&[usize]>) 
                                where T : Copy + Display + PartialOrd {

  let t: &mut [bool] = as_mut_array(res);
  let l: &[T] = as_array(lhs);
  let r: T = *first_value(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = *l.get_unchecked(sel_id) > r;
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = *l.get_unchecked(i) > r;
      }
    }
  }
}

pub fn map_gt_cv<T>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                      selected: Option<&[usize]>) 
                      where T : Copy + Display + PartialOrd {

  let t: &mut [bool] = as_mut_array(res);
  let l: T = *first_value(lhs);
  let r: &[T] = as_array(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = l > *r.get_unchecked(sel_id);
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = l > *r.get_unchecked(i);
      }
    }
  }
}


// Map::Comp::Ge -------------------------------------------------------------
pub fn map_ge_vv<T>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                                selected: Option<&[usize]>) 
                                where T : Copy + Display + PartialOrd {
  let t: &mut [bool] = as_mut_array(res);
  let l: &[T] = as_array(lhs);
  let r: &[T] = as_array(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = *l.get_unchecked(sel_id) >= *r.get_unchecked(sel_id);
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = *l.get_unchecked(i) >= *r.get_unchecked(i);
      }
    }
  }
}

pub fn map_ge_vc<T>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                                selected: Option<&[usize]>) 
                                where T : Copy + Display + PartialOrd {

  let t: &mut [bool] = as_mut_array(res);
  let l: &[T] = as_array(lhs);
  let r: T = *first_value(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = *l.get_unchecked(sel_id) >= r;
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = *l.get_unchecked(i) >= r;
      }
    }
  }
}

pub fn map_ge_cv<T>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                      selected: Option<&[usize]>) 
                      where T : Copy + Display + PartialOrd {

  let t: &mut [bool] = as_mut_array(res);
  let l: T = *first_value(lhs);
  let r: &[T] = as_array(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = l >= *r.get_unchecked(sel_id);
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = l >= *r.get_unchecked(i);
      }
    }
  }
}

// Filter::Comp::Lt -------------------------------------------------------------
pub fn filter_lt_vv<T>(res: &mut [usize], lhs: &Vector, rhs: &Vector, 
                                selected: Option<&[usize]>, num: usize) 
                                where T : Copy + Display + PartialOrd {
  let l: &[T] = as_array(lhs);
  let r: &[T] = as_array(rhs);

  let mut matched: usize = 0;

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;

      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];        
        *res.get_unchecked_mut(matched) = sel_id;
        matched = matched + 
          if *l.get_unchecked(sel_id) < *r.get_unchecked(sel_id) { 1 } else { 0 };
      }
    }
  } else {
    unsafe {
      for i in 0..num {
        *res.get_unchecked_mut(matched) = i;
        matched = matched + 
          if *l.get_unchecked(i) < *r.get_unchecked(i) { 1 } else { 0 };
      }
    }
  }
}

// Arithmetic Plus -----------------------------------------------------------

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
        *t.get_unchecked_mut(i) = *l.get_unchecked(i) + *r.get_unchecked(i);
      }
    }
  }
}


pub fn map_plus_vc<T: ops::Add>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                                selected: Option<&[usize]>) 
                                where T : Copy + Display + ops::Add<T, Output=T> {

  let t: &mut [T] = as_mut_array(res);
  let l: &[T] = as_array(lhs);
  let r: T = *first_value(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = *l.get_unchecked(sel_id) + r;
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = *l.get_unchecked(i) + r;
      }
    }
  }
}

pub fn map_plus_cv<T: ops::Add>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                                selected: Option<&[usize]>) 
                                where T : Copy + Display + ops::Add<T, Output=T> {

  let t: &mut [T] = as_mut_array(res);
  let l: T = *first_value(lhs);
  let r: &[T] = as_array(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = l + *r.get_unchecked(sel_id);
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = l + *r.get_unchecked(i);
      }
    }
  }
}

// Arithmetic Subtract -------------------------------------------------------

pub fn map_sub_vv<T: ops::Sub>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                                selected: Option<&[usize]>) 
                                where T : Copy + Display + ops::Sub<T, Output=T> {

  let t: &mut [T] = as_mut_array(res);
  let l: &[T] = as_array(lhs);
  let r: &[T] = as_array(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = *l.get_unchecked(sel_id) - *r.get_unchecked(sel_id);
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = *l.get_unchecked(i) - *r.get_unchecked(i);
      }
    }
  }
}

pub fn map_sub_vc<T: ops::Sub>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                                selected: Option<&[usize]>) 
                                where T : Copy + Display + ops::Sub<T, Output=T> {

  let t: &mut [T] = as_mut_array(res);
  let l: &[T] = as_array(lhs);
  let r: T = *first_value(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = *l.get_unchecked(sel_id) - r;
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = *l.get_unchecked(i) - r;
      }
    }
  }
}

pub fn map_sub_cv<T: ops::Sub>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                                selected: Option<&[usize]>) 
                                where T : Copy + Display + ops::Sub<T, Output=T> {

  let t: &mut [T] = as_mut_array(res);
  let l: T = *first_value(lhs);
  let r: &[T] = as_array(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = l - *r.get_unchecked(sel_id);
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = l - *r.get_unchecked(i);
      }
    }
  }
}


// Arithmetic Multiply -------------------------------------------------------

pub fn map_mul_vv<T: ops::Mul>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                                selected: Option<&[usize]>) 
                                where T : Copy + Display + ops::Mul<T, Output=T> {

  let t: &mut [T] = as_mut_array(res);
  let l: &[T] = as_array(lhs);
  let r: &[T] = as_array(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = *l.get_unchecked(sel_id) * *r.get_unchecked(sel_id);
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = *l.get_unchecked(i) * *r.get_unchecked(i);
      }
    }
  }
}

pub fn map_mul_vc<T: ops::Mul>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                                selected: Option<&[usize]>) 
                                where T : Copy + Display + ops::Mul<T, Output=T> {

  let t: &mut [T] = as_mut_array(res);
  let l: &[T] = as_array(lhs);
  let r: T = *first_value(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = *l.get_unchecked(sel_id) * r;
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = *l.get_unchecked(i) * r;
      }
    }
  }
}

pub fn map_mul_cv<T: ops::Mul>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                                selected: Option<&[usize]>) 
                                where T : Copy + Display + ops::Mul<T, Output=T> {

  let t: &mut [T] = as_mut_array(res);
  let l: T = *first_value(lhs);
  let r: &[T] = as_array(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = l * *r.get_unchecked(sel_id);
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = l * *r.get_unchecked(i);
      }
    }
  }
}

// Arithmetic Divide ---------------------------------------------------------

pub fn map_div_vv<T: ops::Div>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                                selected: Option<&[usize]>) 
                                where T : Copy + Display + ops::Div<T, Output=T> {

  let t: &mut [T] = as_mut_array(res);
  let l: &[T] = as_array(lhs);
  let r: &[T] = as_array(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = *l.get_unchecked(sel_id) / *r.get_unchecked(sel_id);
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = *l.get_unchecked(i) / *r.get_unchecked(i);
      }
    }
  }
}

pub fn map_div_vc<T: ops::Div>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                                selected: Option<&[usize]>) 
                                where T : Copy + Display + ops::Div<T, Output=T> {

  let t: &mut [T] = as_mut_array(res);
  let l: &[T] = as_array(lhs);
  let r: T = *first_value(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = *l.get_unchecked(sel_id) / r;
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = *l.get_unchecked(i) / r;
      }
    }
  }
}

pub fn map_div_cv<T: ops::Div>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                                selected: Option<&[usize]>) 
                                where T : Copy + Display + ops::Div<T, Output=T> {

  let t: &mut [T] = as_mut_array(res);
  let l: T = *first_value(lhs);
  let r: &[T] = as_array(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = l / *r.get_unchecked(sel_id);
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = l / *r.get_unchecked(i);
      }
    }
  }
}


// Arithmetic Remain ---------------------------------------------------------

pub fn map_rem_vv<T: ops::Rem>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                                selected: Option<&[usize]>) 
                                where T : Copy + Display + ops::Rem<T, Output=T> {

  let t: &mut [T] = as_mut_array(res);
  let l: &[T] = as_array(lhs);
  let r: &[T] = as_array(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = *l.get_unchecked(sel_id) % *r.get_unchecked(sel_id);
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = *l.get_unchecked(i) % *r.get_unchecked(i);
      }
    }
  }
}

pub fn map_rem_vc<T: ops::Rem>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                                selected: Option<&[usize]>) 
                                where T : Copy + Display + ops::Rem<T, Output=T> {

  let t: &mut [T] = as_mut_array(res);
  let l: &[T] = as_array(lhs);
  let r: T = *first_value(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = *l.get_unchecked(sel_id) % r;
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = *l.get_unchecked(i) % r;
      }
    }
  }
}

pub fn map_rem_cv<T: ops::Rem>(res: &mut Vector, lhs: &Vector, rhs: &Vector, 
                                selected: Option<&[usize]>) 
                                where T : Copy + Display + ops::Rem<T, Output=T> {

  let t: &mut [T] = as_mut_array(res);
  let l: T = *first_value(lhs);
  let r: &[T] = as_array(rhs);

  if selected.is_some() {
    let sel_vec = selected.unwrap();
    unsafe {
      let mut sel_id: usize;
      for i in 0..sel_vec.len() {
        sel_id = sel_vec[i];
        *t.get_unchecked_mut(sel_id) = l % *r.get_unchecked(sel_id);
      }
    }
  } else {
    unsafe {
      for i in 0..VECTOR_SIZE {
        *t.get_unchecked_mut(i) = l % *r.get_unchecked(i);
      }
    }
  }
}