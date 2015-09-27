use std::fmt;
use std::ops::Index;
use std::slice::Iter;
use std::vec::Vec;

use itertools::Itertools;

use types::*;

pub type ColumnId = usize;

#[derive(Clone, PartialEq, Debug)]
pub struct Field {
  pub name: String,
  pub decl: FieldDecl
}

impl Field {
  pub fn scalar(name: String, ty: Ty) -> Field {
    Field {
      name: name,
      decl: FieldDecl::Scalar(ty)
    }
  }

  pub fn record(name: String, r: Record) -> Field {
    Field {
      name: name,
      decl: FieldDecl::Record(r)
    }
  }

  pub fn array(name: String, decl: FieldDecl) -> Field {
    Field {
      name: name,
      decl: FieldDecl::Array(Box::new(decl))
    }
  }

  pub fn map(name: String, key_ty: FieldDecl, val_ty: FieldDecl) -> Field {
    Field {
      name: name,
      decl: FieldDecl::Map(Box::new(key_ty), Box::new(val_ty))
    }
  }

  pub fn name(&self) -> &str {
    &self.name
  }
}

impl fmt::Display for Field {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", display_field(self))
  }
}

fn display_field(f: &Field) -> String {
  format!("{} {}", f.name, f.decl)
}

#[derive(Clone, PartialEq, Debug)]
pub enum FieldDecl {
  Scalar (Ty),
  Record (Record),
  Array  (Box<FieldDecl>),
  Map    (Box<FieldDecl>, Box<FieldDecl>)
}

impl FieldDecl {
  pub fn from_fields(fields: Vec<Field>) -> FieldDecl {
    FieldDecl::Record(Record::new(fields))
  }
}

impl fmt::Display for FieldDecl {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", display_field_decl(self))
  }
}

fn display_field_decl(decl: &FieldDecl) -> String {
  match *decl {
    FieldDecl::Scalar(ref ty)         => ty.name().to_owned(),
    FieldDecl::Record(ref r)          => format!("record ({})", r),
    FieldDecl::Array (ref vt)         => format!("array<{}>", vt),
    FieldDecl::Map   (ref kt, ref vt) => format!("map<{},{}>", kt, vt)
  }
}

#[derive(Clone, PartialEq, Debug)]
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
    self.fields.iter().position(|f| f.name == name)
  }

  pub fn find_by_name(&self, name : &str) -> Option<&Field> {
    self.fields.iter().filter(|&f| f.name == name).next()
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
    write!(f, "{}", display_fields(&self.fields))
  }
}

#[inline]
fn display_fields(fields: &Vec<Field>) -> String {
  fields.iter().map(|f| display_field(f)).join(", ")
}

#[allow(dead_code)]
fn create_test_schema() -> Record {
	Record::new(vec![
    Field::scalar("col1".to_owned(), *INT4_TY),
    Field::record("col2".to_owned(), Record::new(
      vec![
        Field::scalar("col3".to_owned(), *INT4_TY),
        Field::scalar("col4".to_owned(), *INT4_TY)
      ])
    ),
    Field::record(
        "col5".to_owned(), Record::new(
      vec![
        Field::array("col6".to_owned(), FieldDecl::Scalar(*INT8_TY)),
        Field::array("col7".to_owned(), FieldDecl::Scalar(*INT8_TY)),
      ])
    ),
    Field::map("col8".to_owned(), FieldDecl::Scalar(*INT4_TY), FieldDecl::from_fields(
      vec![
        Field::scalar("col9".to_owned(), *TEXT_TY),
        Field::scalar("col10".to_owned(), *TEXT_TY),
      ])
    )
  ])
}

#[test]
fn test_schema_creation() {
  let schema = create_test_schema();
  assert_eq!(
    "col1 int4, col2 record (col3 int4, col4 int4), col5 record (col6 array<int8>, col7 array<int8>), col8 map<int4,record (col9 text, col10 text)>".to_string(), 
    format!("{}", schema)); 
}
