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
  VARCHAR,
  TEXT,
  DATE,
  TIME,
  TIMESTAMP,
  BLOB
}

/// Data Domain for each field
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct DataType {
  class : TypeClass, 
  length : u32, // for CHAR, VARCHAR
  precision : u8, // for numeric or decimal
  scale : u8 // for numeric or decimal
}

impl DataType {
  pub fn new (class : TypeClass) -> DataType {
      return DataType {class: class, length : 0, precision: 0, scale: 0};
  }

pub fn class(&self) -> TypeClass {
   self.class
  }
}