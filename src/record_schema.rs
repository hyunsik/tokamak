use std::slice::Iter;
use std::vec::Vec;

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

pub fn print_root_record(r: &Record) {
  for f in &r.fields {

    match *f {
      Field::Scalar(ref name, ref ty) => println!("{}", name),
      Field::Record(ref name, ref fields) => print_record(name, fields, 1),
      Field::Array(ref name, ref fields) => println!("{}", name),
      Field::Map(ref name, ref ty, ref fields) => println!("{}", name)
    };
}
}

pub fn print_record(name: &str, record: &Vec<Field>, depth: u16) {

}
