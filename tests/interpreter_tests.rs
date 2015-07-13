extern crate tajo;

use std::{i8,i16};

use tajo::schema::*;
use tajo::eval::{Eval, MapEval};
use tajo::eval::interpreter::*;
use tajo::rows::*;
use tajo::rows::vector::as_array;
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

#[test]
fn test_field_eval() {
  let schema = make_test_schema();
  
  let mut field: Box<MapEval> = Box::new(Field::new(&Column::new("c3".to_string(), Ty::Int4)));
  let mut r: Box<RowBlockWriter> = Box::new(HeapVRowBlock::new(schema.clone()));
  fill_vector_block(&mut *r);  

  assert!(field.bind(&schema).is_ok());
  let v: &Vector = field.eval(r.as_reader());
  let array: &[INT4_T] = as_array(v);

  assert_eq!(Ty::Int4, v.data_ty().ty());
  assert_eq!(1024, array.iter().count());
}