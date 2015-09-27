use std::fmt;
use std::ops::Index;
use std::slice::Iter;
use std::vec::Vec;

use itertools::Itertools;

use types::*;

pub type ColumnId = usize;


#[derive(Clone, PartialEq, Debug)]
pub struct Constraint 
{
  pub nullable: bool,
  pub unique: bool,
  pub sorted: bool
}


pub struct Identifier 
{
  pub name: String,
  pub quoted: bool
}

impl Identifier 
{
  pub fn new<T: AsRef<str>>(name: T) -> Identifier 
  {
    Identifier {
      name: name.as_ref().to_string(), 
      quoted: false
    }
  }
    
  pub fn new_quoted<T: AsRef<str>>(name: T, quoted: bool) -> Identifier 
  {
    Identifier {
      name: name.as_ref().to_string(), 
      quoted: quoted
    }
  }
}


#[derive(Clone, PartialEq, Debug)]
pub struct Field 
{
  pub name: String,
  pub decl: FieldDecl,
  pub cstr: Option<Constraint> 
}

impl Field 
{
  pub fn new<T: AsRef<str>>(name: T, decl: FieldDecl) -> Field 
  {
    Field {
      name: name.as_ref().to_string(),
      decl: decl,
      cstr: None,
    }
  }
  
  pub fn scalar<T: AsRef<str>>(name: T, ty: Ty) -> Field 
  {
    Field::new(name, FieldDecl::Scalar(ty))
  }
  
  pub fn array<T: AsRef<str>>(name: T, decl: FieldDecl) -> Field 
  {
    Field::new(name, FieldDecl::Array(Box::new(decl)))
  }
  
  pub fn map<T: AsRef<str>>(name: T, key_ty: FieldDecl, val_ty: FieldDecl) -> Field 
  {
    Field::new(name, FieldDecl::Map(Box::new(key_ty), Box::new(val_ty)))
  }
  
  pub fn record<T: AsRef<str>>(name: T, r: Record) -> Field 
  {
    Field::new(name, FieldDecl::Record(r))
  }
  
  pub fn record_from_vec<T: AsRef<str>>(name: T, fields: Vec<Field>) -> Field 
  {
    Field::new(name, FieldDecl::Record(Record::new(fields)))
  }
  
  #[inline]
  pub fn name(&self) -> &str { &self.name }
}


impl fmt::Display for Field 
{
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result 
  {
    write!(f, "{}", display_field(self))
  }
}

#[inline]
fn display_field(f: &Field) -> String { format!("{} {}", f.name, f.decl) }

#[inline]
fn display_fields(fields: &Vec<Field>) -> String 
{
  fields.iter().map(|f| display_field(f)).join(", ")
}


#[derive(Clone, PartialEq, Debug)]
pub enum FieldDecl 
{
  Scalar (Ty),
  Record (Record),
  Array  (Box<FieldDecl>),
  Map    (Box<FieldDecl>, Box<FieldDecl>)
}

impl FieldDecl 
{
  pub fn scalar(ty: Ty) -> FieldDecl { FieldDecl::Scalar(ty) }
  
  pub fn record_from_vec(fields: Vec<Field>) -> FieldDecl 
  { 
    FieldDecl::Record(Record::new(fields))
  }
}

impl fmt::Display for FieldDecl 
{
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result 
  {
    write!(f, "{}", display_field_decl(self))
  }
}

fn display_field_decl(decl: &FieldDecl) -> String 
{
  match *decl {
    FieldDecl::Scalar(ref ty)         => ty.name().to_owned(),
    FieldDecl::Record(ref r)          => format!("record ({})", r),
    FieldDecl::Array (ref vt)         => format!("array<{}>", vt),
    FieldDecl::Map   (ref kt, ref vt) => format!("map<{},{}>", kt, vt)
  }
}


#[derive(Clone, PartialEq, Debug)]
pub struct Record { fields: Vec<Field> }

impl Record 
{
  pub fn new(fields: Vec<Field>) -> Record { Record {fields: fields} }

  pub fn size(&self) -> usize { self.fields.len() }

  pub fn find_id(&self, name: &str) -> Option<ColumnId> 
  {
    self.fields.iter().position(|f| f.name == name)
  }

  pub fn find_by_name(&self, name : &str) -> Option<&Field> 
  {
    self.fields.iter().filter(|&f| f.name == name).next()
  }

  pub fn iter(&self) -> Iter<Field> 
  {
    self.fields.iter()
  }
}

impl Index<ColumnId> for Record 
{
  type Output = Field;
  fn index<'a>(&'a self, id: ColumnId) -> &'a Field 
  {
    debug_assert!(id < self.fields.len(), "Field index is out of range");
    &self.fields[id]
  }
}

impl fmt::Display for Record 
{
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result 
  {
    write!(f, "{}", display_fields(&self.fields))
  }
}


#[allow(dead_code)]
fn create_test_schema() -> Record 
{
	Record::new(vec![
    Field::scalar("col1", *INT4_TY),
    Field::record_from_vec("col2", vec![
        Field::scalar("col3", *INT4_TY),
        Field::scalar("col4", *INT4_TY)
      ]
    ),
    Field::record_from_vec("col5", vec![
        Field::array("col6", FieldDecl::scalar(*INT8_TY)),
        Field::array("col7", FieldDecl::scalar(*INT8_TY)),
      ]
    ),
    Field::map("col8", FieldDecl::scalar(*INT4_TY), FieldDecl::record_from_vec(
      vec![
        Field::scalar("col9",  *TEXT_TY),
        Field::scalar("col10", *TEXT_TY),
      ])
    )
  ])
}

#[test]
fn test_schema_creation() 
{
  let schema = create_test_schema();
  assert_eq!(vec![
    "col1 int4, col2 record (col3 int4, col4 int4), col5 record (col6 array<int8>, ",
    "col7 array<int8>), col8 map<int4,record (col9 text, col10 text)>"
    ].concat(), format!("{}", schema)); 
}
