use common::types::TypeKind;
use common::types::DataType;


#[derive(Clone, PartialEq, Debug)]
pub struct Column {
  pub name: String,
  pub data_type: DataType,
}

impl Column {
  pub fn new(column_name: String, type_class: TypeKind) -> Column {
    Column {name: column_name, data_type: DataType::new(type_class)}
  }

  pub fn new_with_len(column_name: String, type_class: TypeKind, len: u32) -> Column {
    Column {name: column_name, data_type: DataType::new_vartype(type_class, len)}
  }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Schema {
  columns : Vec<Column>
}

impl Schema {
  pub fn new(columns: Vec<Column>) -> Schema {
    Schema {columns: columns}
  }

  pub fn size(&self) -> usize {
    self.columns.len()
  }

  pub fn add(&mut self, c : Column) {
    self.columns.push(c);
  }

  pub fn add_directly(&mut self, name : String, type_class: TypeKind) {
    self.columns.push(Column::new(name, type_class));
  }

  pub fn column(&self, idx : usize) -> &Column {
    debug_assert!(idx < self.columns.len(), "Column index out of range");
    &self.columns[idx]
  }

  pub fn columns(&self) -> &Vec<Column> {
    &self.columns
  }

  pub fn get_by_name(&self, name : String) -> Option<&Column> {
    self.columns.iter().filter(|&c| c.name == name).next()
  }
}