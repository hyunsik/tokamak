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
      decl: decl
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

/*
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
*/
