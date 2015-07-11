extern crate tajo;

use std::{i8,i16};

use tajo::common::*;
use tajo::types::*;
use tajo::schema::*;
use tajo::rows::*;
use std::boxed::Box;

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

  Schema::new(columns)
}

pub fn fill_vector_block(rowblock: &mut RowBlock) {
  for i in (0..1024) {
    rowblock.put_int1(1, i, (i % (i8::MAX - 1) as usize) as i8);
    rowblock.put_int2(2, i, (i % (i16::MAX -1) as usize) as i16);
    rowblock.put_int4(3, i, i as i32);
    rowblock.put_int8(4, i, i as i64);
    rowblock.put_float4(5, i, i as f32);
    rowblock.put_float8(6, i, i as f64);
    rowblock.put_date(7, i, i as i32);
    rowblock.put_time(8, i, i as i64);
    rowblock.put_timestamp(9, i, i as i64);
    
    rowblock.put_text(10, i, &"Rust");
  }
}

fn verify_vector_block(rowblock: &RowBlock) {
  for i in (0..1024) {
    assert_eq!(rowblock.get_int1(1, i), (i % (i8::MAX - 1) as usize) as i8);
    assert_eq!(rowblock.get_int2(2, i), (i % (i16::MAX - 1) as usize) as i16);
    assert_eq!(rowblock.get_int4(3, i), i as i32);
    assert_eq!(rowblock.get_int8(4, i), i as i64);
    assert_eq!(rowblock.get_float4(5, i), i as f32);
    assert_eq!(rowblock.get_float8(6, i), i as f64);
    assert_eq!(rowblock.get_date(7, i), i as i32);
    assert_eq!(rowblock.get_time(8, i), i as i64);
    assert_eq!(rowblock.get_timestamp(9, i), i as i64);

    assert_eq!(
      *(rowblock.get_text(10, i)), StringSlice::new_from_str("Rust"));
  }
}

#[test]
fn allocated_vector_init() { 
  let schema = make_test_schema();
  
  let mut rowblock = AllocatedVecRowBlock::new(schema);
  fill_vector_block(&mut rowblock);
  verify_vector_block(&rowblock);
}

#[test]
fn test_rowblock() {
  let mut columns = Vec::new();
  columns.push(Column::new("c1".to_string(), Ty::Int4));
  columns.push(Column::new("c2".to_string(), Ty::Int8));
  columns.push(Column::new("c3".to_string(), Ty::Float4));

  let schema = Schema::new(columns);
  let rowblock: Box<RowBlock> = Box::new(SlotVecRowBlock::new(schema));  

  assert_eq!(rowblock.column_num(), 3);
}