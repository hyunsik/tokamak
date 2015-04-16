extern crate tajo;

use tajo::common::*;
use tajo::tuple::*;

fn make_test_schema() -> Schema {
  let mut columns = Vec::new();
  columns.push(Column::new("c1".to_string(), TypeClass::BOOL));
  columns.push(Column::new("c2".to_string(), TypeClass::INT1));
  columns.push(Column::new("c3".to_string(), TypeClass::INT2));
  columns.push(Column::new("c4".to_string(), TypeClass::INT4));
  columns.push(Column::new("c5".to_string(), TypeClass::INT8));
  columns.push(Column::new("c6".to_string(), TypeClass::FLOAT4));
  columns.push(Column::new("c7".to_string(), TypeClass::FLOAT8));
  columns.push(Column::new("c8".to_string(), TypeClass::DATE));
  columns.push(Column::new("c9".to_string(), TypeClass::TIME));
  columns.push(Column::new("c10".to_string(), TypeClass::TIMESTAMP));

  columns.push(Column::new("c11".to_string(), TypeClass::TEXT));
  columns.push(Column::new_with_len("c12".to_string(), TypeClass::VARCHAR, 4));
  columns.push(Column::new_with_len("c13".to_string(), TypeClass::CHAR, 4));

  Schema::new(columns)
}

#[test]
fn allocated_vector_init() { 
  let schema = make_test_schema();
  
  let rowblock = VecRowBlock {rowblock: AllocatedVecRowBlock::new(schema) };  

  for i in 0..constant::VECTOR_SIZE {
    rowblock.put_int1(1, i, 1);
  }

  for i in 0..constant::VECTOR_SIZE {
    assert_eq!(rowblock.get_int1(1, i), 1);
  }
}

#[test]
fn test_rowblock() {
  let mut columns = Vec::new();
  columns.push(Column::new("c1".to_string(), TypeClass::INT4));
  columns.push(Column::new("c2".to_string(), TypeClass::INT8));
  columns.push(Column::new("c3".to_string(), TypeClass::FLOAT4));

  let schema = Schema::new(columns);
  let rowblock = VecRowBlock {rowblock: SlotVecRowBlock::new(schema) };  

  assert_eq!(rowblock.column_num(), 3);
}