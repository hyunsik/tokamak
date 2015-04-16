extern crate tajo;

use tajo::common::*;

#[test]
fn test_column() {
  let type_ = DataType::new(TypeClass::INT2);
  let mut c1 = Column {name: "ABC".to_string(), data_type: type_};
  assert_eq!(c1.name, "ABC".to_string());
  assert_eq!(c1.data_type.class(), TypeClass::INT2);
}