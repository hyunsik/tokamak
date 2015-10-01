use common::types::{Type, TypeId};
use common::rows::{MiniPage, FMiniPage};

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

pub static BOOL:       TypeId = TypeId {name: BOOL_STR};
pub static INT1:       TypeId = TypeId {name: INT1_STR};
pub static INT2:       TypeId = TypeId {name: INT2_STR};
pub static INT4:       TypeId = TypeId {name: INT4_STR};
pub static INT8:       TypeId = TypeId {name: INT8_STR};
pub static FLOAT4:     TypeId = TypeId {name: FLOAT4_STR};
pub static FLOAT8:     TypeId = TypeId {name: FLOAT8_STR};
pub static NUMERIC:    TypeId = TypeId {name: NUMERIC_STR};
pub static DATE:       TypeId = TypeId {name: DATE_STR};
pub static TIME:       TypeId = TypeId {name: TIME_STR};
pub static TIMEZ:      TypeId = TypeId {name: TIMEZ_STR};
pub static TIMESTAMP:  TypeId = TypeId {name: TIMESTAMP_STR};
pub static TIMESTAMPZ: TypeId = TypeId {name: TIMESTAMPZ_STR};
pub static INTERVAL:   TypeId = TypeId {name: INTERVAL_STR};
pub static CHAR:       TypeId = TypeId {name: CHAR_STR};
pub static BINARY:     TypeId = TypeId {name: BINARY_STR};
pub static CLOB:       TypeId = TypeId {name: CLOB_STR};
pub static BLOB:       TypeId = TypeId {name: BLOB_STR};

#[derive(Clone, PartialEq, Debug)]
pub struct Int4Type;

impl Type for Int4Type {
  #[inline]
  fn id(&self) -> &TypeId { &INT4 }
  #[inline]
  fn display_name(&self) -> &str { &INT4.name }
  #[inline]
  fn is_comparable(&self) -> bool { true }
  #[inline]
  fn is_orderable(&self) -> bool { true }
  #[inline]
  fn type_params(&self) -> Vec<&Type> { Vec::new() }
//  #[inline]
//  fn hash_fn(&self) -> Box<FnMut(&Vector, &mut [u32])>;
  #[inline]
  fn create_minipage (&self) -> Box<MiniPage> {
    Box::new(FMiniPage::new(Box::new(Int4Type)))
  }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Float4Type;

impl Type for Float4Type {
  #[inline]
  fn id(&self) -> &TypeId { &FLOAT4 }
  #[inline]
  fn display_name(&self) -> &str { &FLOAT4.name }
  #[inline]
  fn is_comparable(&self) -> bool { true }
  #[inline]
  fn is_orderable(&self) -> bool { true }
  #[inline]
  fn type_params(&self) -> Vec<&Type> { Vec::new() }
//  #[inline]
//  fn hash_fn(&self) -> Box<FnMut(&Vector, &mut [u32])>;
  #[inline]
  fn create_minipage (&self) -> Box<MiniPage> {
    Box::new(FMiniPage::new(Box::new(Int4Type)))
  }
}

#[test]
pub fn test_int4() {
  //let page_builder: Box<PageBuilder> = Box::new(DefaultPageBuilder);
//  let page = page_builder.create(&vec![
//    Box::new(Int4Type)
//  ]);
//  println!("{}", page.column_num());
}