extern crate tajo;

use tajo::common::schema::*;
use tajo::common::types::*;

fn make_test_schema() -> Schema {
  let mut columns = Vec::new();
  columns.push(Column::new("c0".to_string(), TypeKind::Bool));      // 0
  columns.push(Column::new("c1".to_string(), TypeKind::Int1));      // 1
  columns.push(Column::new("c2".to_string(), TypeKind::Int2));      // 2
  columns.push(Column::new("c3".to_string(), TypeKind::Int4));      // 3
  columns.push(Column::new("c4".to_string(), TypeKind::Int8));      // 4
  columns.push(Column::new("c5".to_string(), TypeKind::Float4));    // 5
  columns.push(Column::new("c6".to_string(), TypeKind::Float8));    // 6
  columns.push(Column::new("c7".to_string(), TypeKind::Date));      // 7
  columns.push(Column::new("c8".to_string(), TypeKind::Time));      // 8
  columns.push(Column::new("c9".to_string(), TypeKind::Timestamp)); // 9

  columns.push(Column::new("c10".to_string(), TypeKind::Text));     // 10
  // columns.push(Column::new_with_len("c12".to_string(), TypeKind::VARCHAR, 4));
  // columns.push(Column::new_with_len("c13".to_string(), TypeKind::CHAR, 4));

  Schema::new(columns)
}

#[test]
fn test_column() {
  let type_ = DataType::new(TypeKind::Int2);
  let c1 = Column {name: "ABC".to_string(), data_type: type_};
  assert_eq!(c1.name, "ABC".to_string());
  assert_eq!(c1.data_type.class(), TypeKind::Int2);
}

#[test]
fn test_column_id() {
  let schema = make_test_schema();

  assert_eq!(3usize, schema.column_id("c3").unwrap());
  assert_eq!(9usize, schema.column_id("c9").unwrap());

  assert!(schema.column_id("err").is_none());
}

