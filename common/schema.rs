use std::fmt;
use std::ops::Index;
use std::slice::Iter;
use std::vec::Vec;

use itertools::Itertools;

use types::*;

pub type ColumnId = usize;

#[derive(Clone, PartialEq, Debug)]
pub enum Field {
  Scalar (String, Ty),
  Record (String, Vec<Field>),
  Array (String, Vec<Field>),
  Map (String, Ty, Vec<Field>)
}

impl Field {
  pub fn name(&self) -> &str {
    match *self {
      Field::Scalar(ref name, ref ty)              => name,
      Field::Record(ref name, ref fields)          => name,
      Field::Array(ref name, ref fields)           => name,
      Field::Map(ref name, ref key_ty, ref fields) => name
    }
  }
}

impl fmt::Display for Field {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", display_field(self))
  }
}

pub struct Record {
  fields: Vec<Field>
}

impl Record {
  pub fn new(fields: Vec<Field>) -> Record {
    Record {fields: fields}
  }

  pub fn size(&self) -> usize {
    self.fields.len()
  }

  pub fn find_id(&self, name: &str) -> Option<ColumnId> {
    self.fields.iter().position(|f| f.name() == name)
  }

  pub fn find_by_name(&self, name : &str) -> Option<&Field> {
    self.fields.iter().filter(|&f| f.name() == name).next()
  }

  pub fn iter(&self) -> Iter<Field> {
    self.fields.iter()
  }
}

impl Index<ColumnId> for Record {
  type Output = Field;
  fn index<'a>(&'a self, id: ColumnId) -> &'a Field {
    debug_assert!(id < self.fields.len(), "Field index is out of range");
    &self.fields[id]
  }
}

impl fmt::Display for Record {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", display_record(self))
  }
}

#[inline]
pub fn display_record(r: &Record) -> String {
  display_fields(&r.fields)
}

fn display_field(f: &Field) -> String {
  match *f {
    Field::Scalar(ref name, ref ty)     => format!("{} {}", name, ty),
    Field::Record(ref name, ref fields) => format!("{} record ({})", name, display_fields(fields)),
    Field::Array(ref name, ref fields)  => format!("{} array<{}>", name, display_fields(fields)),
    Field::Map(ref name, ref key_ty, ref fields) => {
        format!("{} map<{},{}>", name, key_ty, display_fields(fields))
    }
  }
}

#[inline]
fn display_fields(fields: &Vec<Field>) -> String {
  fields.iter().map(|f| display_field(f)).join(", ")
}


#[test]
fn test_creation() {
    let mut fields = Vec::new();
    fields.push(Field::Scalar("col1".to_owned(), *INT4_TY));
    fields.push(Field::Record("col2".to_owned(), vec![
        Field::Scalar("col3".to_owned(), *INT4_TY),
        Field::Scalar("col4".to_owned(), *INT4_TY)
    ]));

    let r = Record::new(fields);
    println!("{}", r);
}
