use super::types::*;

/// Datum representation for a single value
#[derive(Clone)]
pub enum Datum 
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

impl HasTy for Datum 
{
  #[allow(unused_variables)]
  fn data_ty(&self) -> &Ty 
  {
    match *self {      
      Datum::Bool(ref x) => &BOOL_TY,
      Datum::Int1(ref x) => &INT1_TY,
      Datum::Int2(ref x) => &INT2_TY,
      Datum::Int4(ref x) => &INT4_TY,
      Datum::Int8(ref x) => &INT8_TY,
      Datum::Float4(ref x) => &FLOAT4_TY,
      Datum::Float8(ref x) => &FLOAT8_TY,
      Datum::Time(ref x) => &TIME_TY,
      Datum::Date(ref x) => &DATE_TY,
      Datum::Timestamp(ref x) => &TIMESTAMP_TY,
      Datum::Interval(ref x,ref y) => &INTERVAL_TY,
      //Datum::Char(ref x) => &CHAR_TY,
      Datum::Text(ref x) => &TEXT_TY,
      //Datum::Varchar(ref x) => &VARCHAR_TY,
      //Datum::Blob(ref x) => &BLOB_TY
      _ => panic!("Unsupported type")
    }
  }
}
