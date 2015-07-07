use std::mem;
use common::string_slice::StringSlice;

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum TypeKind {
  Bool,  
  Int1,
  Int2,
  Int4,
  Int8,
  Float4,
  Float8,  
  Date,
  Time,
  Timestamp,
  Interval,
  Char,
  Varchar,
  Text,
  Blob
}

pub trait HasTypeKind {
  fn type_kind(&self) -> TypeKind;
}

 #[allow(non_camel_case_types)]
pub type BOOL_T = bool;
#[allow(non_camel_case_types)]
pub type INT1_T = i8;
#[allow(non_camel_case_types)]
pub type INT2_T = i16;
#[allow(non_camel_case_types)]
pub type INT4_T = i32;
#[allow(non_camel_case_types)]
pub type INT8_T = i64;
#[allow(non_camel_case_types)]
pub type FLOAT4_T = f32;
#[allow(non_camel_case_types)]
pub type FLOAT8_T = f64;
#[allow(non_camel_case_types)]
pub type DATE_T = i32;
#[allow(non_camel_case_types)]
pub type TIME_T = i64;
#[allow(non_camel_case_types)]
pub type TIMESTAMP_T = i64;
#[allow(non_camel_case_types)]
pub type TEXT_T = StringSlice;

/// Data Domain for each field
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct DataType {
  class : TypeKind, 
  len : u32, // for CHAR, VARCHAR
  precision : u8, // for numeric or decimal
  scale : u8 // for numeric or decimal
}

impl DataType {
  pub fn new (class : TypeKind) -> DataType {
    return DataType {class: class, len : 0, precision: 0, scale: 0};
  }

  pub fn new_vartype(class : TypeKind, len: u32) -> DataType {
    return DataType {class: class, len : len, precision: 0, scale: 0};
  }

  pub fn bytes_len(&self) -> u32 {
    DataType::size_of(self)
  }

  pub fn class(&self) -> TypeKind {
   self.class
 }

 #[inline(always)]
 pub fn size_of(data_type: &DataType) -> u32 {
  match data_type.class {
    TypeKind::Bool => 1,        
    TypeKind::Int1 => 1,
    TypeKind::Int2 => 2,
    TypeKind::Int4 => 4,
    TypeKind::Int8 => 8,
    TypeKind::Float4 => 4,
    TypeKind::Float8 => 8,
    TypeKind::Date => 4,
    TypeKind::Time => 8,
    TypeKind::Timestamp => 8,
    TypeKind::Interval => 12,
    TypeKind::Char => data_type.len,
    TypeKind::Text => mem::size_of::<TEXT_T>()as u32,
    TypeKind::Varchar | TypeKind::Blob => 12,
  }
}

pub fn has_length(data_type: &DataType) -> bool {
  match data_type.class {
    TypeKind::Char | TypeKind::Varchar | TypeKind::Blob => true,
    _ => false
  }
}

pub fn is_variable(data_type: &DataType) -> bool {
  match data_type.class {
    TypeKind::Varchar | TypeKind::Blob => true,
    _ => false
  }
}

}