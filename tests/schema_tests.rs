extern crate tajo;

use tajo::schema::*;
use tajo::types::*;

fn make_test_schema() -> Schema {
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

#[test]
fn test_column() {
  let c1 = Column {name: "ABC".to_string(), data_ty: DataType::new(Ty::Int2)};
  assert_eq!(c1.name, "ABC".to_string());
  assert_eq!(c1.data_ty.ty, Ty::Int2);
}

#[test]
fn test_column_id() {
  let schema = make_test_schema();

  assert_eq!(3usize, schema.column_id("c3").unwrap());
  assert_eq!(9usize, schema.column_id("c9").unwrap());

  assert!(schema.column_id("err").is_none());
}

