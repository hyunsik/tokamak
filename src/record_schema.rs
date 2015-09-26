use std::fmt;
use std::slice::Iter;
use std::vec::Vec;

use itertools::Itertools;

use types::*;

pub enum Field {
  Scalar (String, Ty),
  Record (String, Vec<Field>),
  Array (String, Vec<Field>),
  Map (String, Ty, Vec<Field>)
}

pub struct Record {
  fields: Vec<Field>
}

impl Record {
  pub fn new(fields: Vec<Field>) -> Record {
    Record {fields: fields}
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
