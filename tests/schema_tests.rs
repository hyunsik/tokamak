extern crate tajo;

use tajo::common::schema::*;
use tajo::common::types::*;

#[test]
fn test_column() {
  let type_ = DataType::new(TypeKind::Int2);
  let c1 = Column {name: "ABC".to_string(), data_type: type_};
  assert_eq!(c1.name, "ABC".to_string());
  assert_eq!(c1.data_type.class(), TypeKind::Int2);
}