use std::mem;

use common::types::{Type, TypeId, TypeHandler};
use common::rows::{MiniPage};
use common::str::{StrSlice};
use rows::fixed_len::FMiniPage;

const BOOL_STR       : &'static str = "bool";
const INT1_STR       : &'static str = "int1";
const INT2_STR       : &'static str = "int2";
const INT4_STR       : &'static str = "int4";
const INT8_STR       : &'static str = "int8";
const FLOAT4_STR     : &'static str = "float4";
const FLOAT8_STR     : &'static str = "float8";
const NUMERIC_STR    : &'static str = "numeric";
const DATE_STR       : &'static str = "date";
const TIME_STR       : &'static str = "time";
const TIMEZ_STR      : &'static str = "time with timezone";
const TIMESTAMP_STR  : &'static str = "timestamp";
const TIMESTAMPZ_STR : &'static str = "timestamp with timezone";
const INTERVAL_STR   : &'static str = "interval";
const CHAR_STR       : &'static str = "char";
const BINARY_STR     : &'static str = "binary";
const CLOB_STR       : &'static str = "clob";
const BLOB_STR       : &'static str = "blob";

//pub const BOOL:       TypeId = TypeId {base: String::from(BOOL_STR)};
//pub const INT1:       TypeId = TypeId {base: INT1_STR.to_string()};
//pub const INT2:       TypeId = TypeId {base: INT2_STR.to_string()};
//pub const INT4:       TypeId = TypeId {base: INT4_STR.to_string()};
//pub const INT8:       TypeId = TypeId {base: INT8_STR.to_string()};
//pub const FLOAT4:     TypeId = TypeId {base: FLOAT4_STR.to_string()};
//pub const FLOAT8:     TypeId = TypeId {base: FLOAT8_STR.to_string()};
//pub const NUMERIC:    TypeId = TypeId {base: NUMERIC_STR.to_string()};
//pub const DATE:       TypeId = TypeId {base: DATE_STR.to_string()};
//pub const TIME:       TypeId = TypeId {base: TIME_STR.to_string()};
//pub const TIMEZ:      TypeId = TypeId {base: TIMEZ_STR.to_string()};
//pub const TIMESTAMP:  TypeId = TypeId {base: TIMESTAMP_STR.to_string()};
//pub const TIMESTAMPZ: TypeId = TypeId {base: TIMESTAMPZ_STR.to_string()};
//pub const INTERVAL:   TypeId = TypeId {base: INTERVAL_STR.to_string()};
//pub const CHAR:       TypeId = TypeId {base: CHAR_STR.to_string()};
//pub const BINARY:     TypeId = TypeId {base: BINARY_STR.to_string()};
//pub const CLOB:       TypeId = TypeId {base: CLOB_STR.to_string()};
//pub const BLOB:       TypeId = TypeId {base: BLOB_STR.to_string()};

#[allow(non_camel_case_types)]
pub type BOOL_T      = bool;
#[allow(non_camel_case_types)]
pub type INT1_T      = i8;
#[allow(non_camel_case_types)]
pub type INT2_T      = i16;
#[allow(non_camel_case_types)]
pub type INT4_T      = i32;
#[allow(non_camel_case_types)]
pub type INT8_T      = i64;
#[allow(non_camel_case_types)]
pub type FLOAT4_T    = f32;
#[allow(non_camel_case_types)]
pub type FLOAT8_T    = f64;
#[allow(non_camel_case_types)]
pub type DATE_T      = i32;
#[allow(non_camel_case_types)]
pub type TIME_T      = i64;
#[allow(non_camel_case_types)]
pub type TIMESTAMP_T = i64;
#[allow(non_camel_case_types)]
pub type TEXT_T      = StrSlice;  

#[derive(Clone, PartialEq, Debug)]
pub struct Int4 
{
  id: TypeId
}

impl Int4 
{
  pub fn new() -> Self
  {
    Int4 {id: TypeId {base: String::from(INT4_STR)} }
  } 
}

impl Type for Int4 
{
  #[inline]
  fn id(&self) -> &TypeId { &self.id }
  #[inline]
  fn display_name(&self) -> &str { &self.id.base }
  #[inline]
  fn is_comparable(&self) -> bool { true }
  #[inline]
  fn is_orderable(&self) -> bool { true }
  #[inline]
  fn type_params(&self) -> Vec<&Type> { Vec::new() }
//  #[inline]
//  fn hash_fn(&self) -> Box<FnMut(&Vector, &mut [u32])>;
  #[inline]
  fn handler_factory (&self) -> Box<TypeHandler> 
  {
    let f = || -> Box<MiniPage> {Box::new(FMiniPage::new(mem::size_of::<i32>()))};
    
    Box::new(TypeHandler {create_minipage_writer: Box::new(f)})
  }
}
 

#[derive(Clone, PartialEq, Debug)]
pub struct Float4 
{
  id: TypeId
}

impl Int4 
{
  pub fn new() -> Self {
    Int4 {id: TypeId {base: String::from(FLOAT4_STR)}}
  }
}

impl Type for Float4 
{
  #[inline]
  fn id(&self) -> &TypeId { &self.id }
  #[inline]
  fn display_name(&self) -> &str { &self.id.base }
  #[inline]
  fn is_comparable(&self) -> bool { true }
  #[inline]
  fn is_orderable(&self) -> bool { true }
  #[inline]
  fn type_params(&self) -> Vec<&Type> { Vec::new() }
//  #[inline]
//  fn hash_fn(&self) -> Box<FnMut(&Vector, &mut [u32])>;
  #[inline]
  fn handler_factory (&self) -> Box<TypeHandler> 
  {
    let f = || -> Box<MiniPage> {Box::new(FMiniPage::new(mem::size_of::<f32>()))};
    
    Box::new(TypeHandler {create_minipage_writer: Box::new(f)})
  }
}