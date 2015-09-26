use std::slice::Iter;
use std::vec::Vec;

use common::types::{Ty, HasTy};

#[derive(Clone, PartialEq, Debug)]
pub struct Column {
  pub name: String,
  pub ty: Ty,
}

impl Column {
  pub fn new<T: AsRef<str>>(name: T, ty: Ty) -> Column {
    Column {
      name: name.as_ref().to_owned(),
      ty: ty
    }
  }
}

impl HasTy for Column {
  fn data_ty(&self) -> &Ty { &self.ty }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Schema {
  columns : Vec<Column>
}

impl Schema {
  pub fn new() -> Schema {
    Schema {columns: Vec::new()}
  }

  pub fn from_vec(columns: Vec<Column>) -> Schema {
    Schema {columns: columns}
  }

  pub fn size(&self) -> usize {
    self.columns.len()
  }

  pub fn add(&mut self, c : Column) {
    self.columns.push(c);
  }

  pub fn add_column(&mut self, name : &str, ty: Ty) {
    self.columns.push(Column::new(name, ty));
  }

  pub fn column_id(&self, name: &str) -> Option<usize> {
    self.columns.iter().position(|c| c.name == name)
  }

  pub fn column(&self, idx : usize) -> &Column {
    debug_assert!(idx < self.columns.len(), "Column index out of range");
    &self.columns[idx]
  }

  pub fn columns(&self) -> &Vec<Column> {
    &self.columns
  }

  pub fn get_by_name(&self, name : &str) -> Option<&Column> {
    self.columns.iter().filter(|&c| c.name == name).next()
  }

  pub fn iter(&self) -> Iter<Column> {
    self.columns.iter()
  }
}

pub mod util {
  use schema::Schema;

  /// Find target column indexes
  pub fn finds_target_indexes(src: &Schema, target: &Schema) -> Vec<usize> {
    let mut indexes: Vec<usize> = Vec::new();

    for c in target.iter() {
      match src.column_id(&c.name) {
        Some(idx) => indexes.push(idx),
        None => {}
      }
    }

    indexes
  }
}
