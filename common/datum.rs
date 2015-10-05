use super::types::*;

/// Datum representation for a single value
#[derive(Clone)]
pub enum ConstValue 
{
  Bool(bool),
  Int1(i8),
  Int2(i16),
  Int4(i32),
  Int8(i64),
  Float4(f32),
  Float8(f64),
  Time(i64),
  Date(i32),
  Timestamp(i64),
  Interval(i64, i32),
  Char(String),
  Text(String),
  Varchar(String),
  Blob(Vec<u8>)
}
