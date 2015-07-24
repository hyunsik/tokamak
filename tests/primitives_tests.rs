extern crate tajo;

use std::{i8,i16};
use std::fmt::Debug;

use tajo::common::StringSlice;
use tajo::schema::*;
use tajo::expr::ArithmOp;
use tajo::eval::{Eval, MapEval};
use tajo::eval::interpreter::*;
use tajo::eval::primitives::*;
use tajo::rows::*;
use tajo::rows::vector::{ArrayVector, as_array, as_mut_array, from_vec};
use tajo::types::*;

pub fn make_test_schema() -> Schema {
  let mut columns = Vec::new();
  columns.push(Column::new("c0".to_string(), Ty::Bool));      // 0
  columns.push(Column::new("c1".to_string(), Ty::Int1));      // 1
  columns.push(Column::new("c2".to_string(), Ty::Int2));      // 2
  columns.push(Column::new("c3".to_string(), Ty::Int4));      // 3
  columns.push(Column::new("c4".to_string(), Ty::Int8));      // 4
  columns.push(Column::new("c5".to_string(), Ty::Float4));    // 5
  columns.push(Column::new("c6".to_string(), Ty::Float8));    // 6
  columns.push(Column::new("c7".to_string(), Ty::Date));      // 7
  columns.push(Column::new("c8".to_string(), Ty::Time));      // 8
  columns.push(Column::new("c9".to_string(), Ty::Timestamp)); // 9

  columns.push(Column::new("c10".to_string(), Ty::Text));     // 10
  // columns.push(Column::new_with_len("c12".to_string(), Ty::VARCHAR, 4));
  // columns.push(Column::new_with_len("c13".to_string(), Ty::CHAR, 4));

  Schema::from_vec(columns)
}

pub fn fill_vector_block(rowblock: &mut RowBlockWriter) {
  for i in (0..1024) {
    rowblock.put_int1(i, 1, (i % (i8::MAX - 1) as usize) as i8);
    rowblock.put_int2(i, 2, (i % (i16::MAX -1) as usize) as i16);
    rowblock.put_int4(i, 3, i as i32);
    rowblock.put_int8(i, 4, i as i64);
    rowblock.put_float4(i, 5, i as f32);
    rowblock.put_float8(i, 6, i as f64);
    rowblock.put_date(i, 7, i as i32);
    rowblock.put_time(i, 8, i as i64);
    rowblock.put_timestamp(i, 9, i as i64);
    
    rowblock.put_text(i, 10, &"Rust");
  }
}

fn verify_vector_block(rowblock: &RowBlock) {
  for i in (0..1024) {
    assert_eq!(rowblock.get_int1(i, 1), (i % (i8::MAX - 1) as usize) as i8);
    assert_eq!(rowblock.get_int2(i, 2), (i % (i16::MAX - 1) as usize) as i16);
    assert_eq!(rowblock.get_int4(i, 3), i as i32);
    assert_eq!(rowblock.get_int8(i, 4), i as i64);
    assert_eq!(rowblock.get_float4(i, 5), i as f32);
    assert_eq!(rowblock.get_float8(i, 6), i as f64);
    assert_eq!(rowblock.get_date(i, 7), i as i32);
    assert_eq!(rowblock.get_time(i, 8), i as i64);
    assert_eq!(rowblock.get_timestamp(i, 9), i as i64);

    assert_eq!(
      *(rowblock.get_text(i, 10)), StringSlice::new_from_str("Rust"));
  }
}

fn test_primitive<T>(f: fn(&mut Vector, &Vector, &Vector, Option<&[usize]>),
                data_ty: &DataTy, 
                lv: Vec<T>, 
                rv: Vec<T>,
                sel: Option<Vec<usize>>,
                expected: Vec<T>) where T: Debug + PartialEq {

  let mut l = from_vec(data_ty, vec![1,2,3,4]);
  let mut r = from_vec(data_ty, vec![1,2,3,4]);

  let mut result = ArrayVector::new(data_ty.clone());
  { f(&mut result, &mut l, &mut r, None); }
    // match sel {
    //   Some(s) => sel.as_array(),
    //   None => None
    // })

  let r: &[T] = as_array(&result);
  for x in 0..lv.len() {
    assert_eq!(expected[x], r[x]);
  }
}

#[test]
fn test_map_plus() {
  test_primitive(
    map_plus_vv::<INT4_T>, 
    &DataTy::new(Ty::Int4), 
    vec![1,2,3,4], vec![1,2,3,4], 
    None, 
    vec![2,4,6,8]
  );
}

#[test]
fn test_filter_lt() {
}