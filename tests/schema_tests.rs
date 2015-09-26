extern crate tajo;
extern crate common;

use self::common::types::*;
use tajo::schema::*;


fn make_test_schema() -> Schema {
  let mut columns = Vec::new();
  columns.push(Column::new("c0".to_string(), *BOOL_TY));      // 0
  columns.push(Column::new("c1".to_string(), *INT1_TY));      // 1
  columns.push(Column::new("c2".to_string(), *INT2_TY));      // 2
  columns.push(Column::new("c3".to_string(), *INT4_TY));      // 3
  columns.push(Column::new("c4".to_string(), *INT8_TY));      // 4
  columns.push(Column::new("c5".to_string(), *FLOAT4_TY));    // 5
  columns.push(Column::new("c6".to_string(), *FLOAT8_TY));    // 6
  columns.push(Column::new("c7".to_string(), *DATE_TY));      // 7
  columns.push(Column::new("c8".to_string(), *TIME_TY));      // 8
  columns.push(Column::new("c9".to_string(), *TIMESTAMP_TY)); // 9

  columns.push(Column::new("c10".to_string(), *TEXT_TY));     // 10
  // columns.push(Column::new_with_len("c12".to_string(), Ty::VARCHAR, 4));
  // columns.push(Column::new_with_len("c13".to_string(), Ty::CHAR, 4));

  Schema::from_vec(columns)
}

#[test]
fn test_column() {
  let c1 = Column {name: "ABC".to_string(), ty: *INT2_TY};
  assert_eq!(c1.name, "ABC".to_string());
  assert_eq!(c1.data_ty(), INT2_TY);
}

#[test]
fn test_column_id() {
  let schema = make_test_schema();

  assert_eq!(3usize, schema.column_id("c3").unwrap());
  assert_eq!(9usize, schema.column_id("c9").unwrap());

  assert!(schema.column_id("err").is_none());
}
