use common::data_type::TypeClass;
use common::data_type::DataType;


#[derive(Clone)]
pub struct Column {
  name: String,
  data_type: DataType,
}

pub struct Schema {
  columns : Vec<Column>
}

impl Schema {
  fn add(&mut self, c : Column) {
    self.columns.push(c);
  }

  fn get(&self, idx : usize) -> &Column {
    debug_assert!(idx < self.columns.len(), "Column index out of range");
    &self.columns[idx]
  }

  fn get_by_name(&self, name : String) -> Option<&Column> {
    self.columns.iter().filter(|&c| c.name == name).next()
  }
}

#[test]
fn test_column() {
  let type_ = DataType::new(TypeClass::INT2);
  let mut c1 = Column {name: "ABC".to_string(), data_type: type_};
  assert_eq!(c1.name, "ABC".to_string());
  assert_eq!(c1.data_type.class(), TypeClass::INT2);
}