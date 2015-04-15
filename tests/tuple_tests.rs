extern crate tajo;

use tajo::common::*;
use tajo::tuple::*;

fn make_test_schema() -> Schema {
  let mut columns = Vec::new();
  columns.push(Column::new("c1".to_string(), TypeClass::INT1));
  columns.push(Column::new("c2".to_string(), TypeClass::INT1));
  columns.push(Column::new("c3".to_string(), TypeClass::INT1));

  Schema::new(columns)
}

#[test]
fn allocated_vector_init() { 
  let schema = make_test_schema();
  
  let rowblock = VecRowBlock {rowblock: AllocatedVecRowBlock::new(schema) };  

  for i in 0..constant::VECTOR_SIZE {
    rowblock.put_int1(0, i, 1);
  }

  for i in 0..constant::VECTOR_SIZE {
    assert_eq!(rowblock.get_int1(0, i), 1);
  }
}