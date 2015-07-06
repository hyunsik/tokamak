use std::mem;
use common::string_slice::StringSlice;

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum TypeClass {
  BOOL,
  CHAR,
  INT1,
  INT2,
  INT4,
  INT8,
  FLOAT4,
  FLOAT8,
  NUMERIC,
  VARCHAR,
  TEXT,
  DATE,
  TIME,
  TIMESTAMP,
  INTERVAL,
  BLOB
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
  class : TypeClass, 
  len : u32, // for CHAR, VARCHAR
  precision : u8, // for numeric or decimal
  scale : u8 // for numeric or decimal
}

impl DataType {
  pub fn new (class : TypeClass) -> DataType {
    return DataType {class: class, len : 0, precision: 0, scale: 0};
  }

  pub fn new_vartype(class : TypeClass, len: u32) -> DataType {
    return DataType {class: class, len : len, precision: 0, scale: 0};
  }

  pub fn bytes_len(&self) -> u32 {
    DataType::size_of(self)
  }

  pub fn class(&self) -> TypeClass {
   self.class
 }

 #[inline(always)]
 pub fn size_of(data_type: &DataType) -> u32 {
  match data_type.class {
    TypeClass::BOOL => 1,    
    TypeClass::CHAR => data_type.len,
    TypeClass::INT1 => 1,
    TypeClass::INT2 => 2,
    TypeClass::INT4 => 4,
    TypeClass::INT8 => 8,
    TypeClass::FLOAT4 => 4,
    TypeClass::FLOAT8 => 8,
    TypeClass::NUMERIC => panic!("NUMERIC is not supported"),
    TypeClass::DATE => 4,
    TypeClass::TIME => 8,
    TypeClass::TIMESTAMP => 8,
    TypeClass::INTERVAL => 12,
    TypeClass::TEXT => mem::size_of::<TEXT_T>()as u32,
    TypeClass::VARCHAR | TypeClass::BLOB => 12,
  }
}

pub fn has_length(data_type: &DataType) -> bool {
  match data_type.class {
    TypeClass::CHAR | TypeClass::VARCHAR | TypeClass::BLOB => true,
    TypeClass::NUMERIC => panic!("NUMERIC is not supported"),
    _ => false
  }
}

pub fn is_variable(data_type: &DataType) -> bool {
  match data_type.class {
    TypeClass::VARCHAR | TypeClass::BLOB => true,
    TypeClass::NUMERIC => panic!("NUMERIC is not supported"),
    _ => false
  }
}

}