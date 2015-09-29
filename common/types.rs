use std::collections::BTreeMap;
use std::fmt;
use std::mem;

use itertools::Itertools;

use rows::{MiniPage, FMiniPage};
use str::StrSlice;

// globally unique id
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct TypeId {
  name: &'static str
}

pub trait TypeManager 
{
  fn get(&self, id: &TypeId) -> Option<&Type>;
  fn types(&self) -> Vec<&Type>;
}

pub struct TypeRegistry 
{
  types: BTreeMap<TypeId, Box<Type>>
}

impl TypeRegistry 
{
  fn new(types: Vec<Box<Type>>) -> TypeRegistry 
  {
    TypeRegistry {
      types: types.into_iter()
        .map(|ty: Box<Type>| { 
            return (*ty.id(), ty) 
        })
        .collect::<BTreeMap<TypeId, Box<Type>>>()    
    }
  }
}

impl TypeManager for TypeRegistry {
  fn get(&self, id: &TypeId) -> Option<&Type> {
    match self.types.get(id) {
      Some(x) => Some(&**x),
      None    => None
    }
  }
  
  fn types(&self) -> Vec<&Type> {
    self.types.values().map(|t| &**t).collect::<Vec<&Type>>()
  }
}

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

pub trait HashFn<T> {
  fn hash(v: &MiniPage, keys: &mut [T]);
}

pub trait HashFnFactory
{
  fn hash32_fn() -> Box<FnMut(i32, &mut [i32])>;
  fn create_hash32_block() -> Box<Fn() -> Box<MiniPage>>;
}

pub trait Type 
{
  fn id                (&self) -> &TypeId;
  fn display_name      (&self) -> &str;
  fn is_comparable     (&self) -> bool;
  fn is_orderable      (&self) -> bool;
  fn type_params       (&self) -> Vec<&Type>;
  //fn hash32_fn         (&self) -> Box<FnMut(&MiniPage, &mut [u32])>;
  fn create_minipage   (&self) -> Box<MiniPage>;
}

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
pub enum Ty 
{
  Bool,
  Int1,
  Int2,
  Int4,
  Int8,
  Float4,
  Float8,
  Numeric(u8, Option<u8>), // precision (1~38), scale (0 <= scale <= precision)
  Date,
  Time,
  Timez,
  Timestamp,
  Timestampz,
  Interval,
  Char(u8),                // its maximum length is 127
  Binary(u8),              // its maximum length is 127
  Clob,
  Blob,
  Array(Box<Ty>),
  Struct(Vec<Ty>),
  Map(Box<Ty>, Box<Ty>)
}

impl Ty 
{
  #[allow(unused_variables)]
  pub fn size_of(&self) -> u32 {
    match *self {
      Ty::Bool                  => 1,
      Ty::Int1                  => 1,
      Ty::Int2                  => 2,
      Ty::Int4                  => 4,
      Ty::Int8                  => 8,
      Ty::Float4                => 4,
      Ty::Float8                => 8,
      Ty::Numeric(ref p, ref s) => panic!("numeric is not supported yet"),
      Ty::Date                  => 4,
      Ty::Time                  => 8,
      Ty::Timez                 => 12,
      Ty::Timestamp             => 8,
      Ty::Timestampz            => 12,
      Ty::Interval              => 12,
      Ty::Char(len)             => len as u32,
      Ty::Binary(len)           => len as u32,
      Ty::Clob                  => mem::size_of::<TEXT>() as u32,
      Ty::Blob                  => mem::size_of::<TEXT>() as u32,
      Ty::Array(ref ty)         => panic!("array is not supported yet"),
      Ty::Struct(ref tys)       => panic!("array is not supported yet"),
      Ty::Map(ref kt, ref vt)   => panic!("map is not supported yet"),
    }
  }
  
  #[allow(unused_variables)]
  pub fn is_variable(&self) -> bool {
    match *self {
      Ty::Char(len) | Ty::Binary(len) => true,
      _ => false
    }
  }
}

impl fmt::Display for Ty {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match *self {
      Ty::Bool                    => write!(f, "bool"),
      Ty::Int1                    => write!(f, "int1"),
      Ty::Int2                    => write!(f, "int2"),
      Ty::Int4                    => write!(f, "int4"),
      Ty::Int8                    => write!(f, "int8"),
      Ty::Float4                  => write!(f, "float4"),
      Ty::Float8                  => write!(f, "float8"),
      Ty::Numeric(ref p,ref s)    => {
        match *s {
          Some(ref scale) => write!(f, "numeric ({}, {})", p, scale),
          None            => write!(f, "numeric ({})", p) 
        }
      }
      Ty::Date                    => write!(f, "date"),
      Ty::Time                    => write!(f, "time"),
      Ty::Timez                   => write!(f, "time with timezone"),
      Ty::Timestamp               => write!(f, "timestamp"),
      Ty::Timestampz              => write!(f, "timeztamp with timezone"),
      Ty::Interval                => write!(f, "interval"),
      Ty::Char(ref len)           => write!(f, "char({})", len),
      Ty::Binary(ref len)         => write!(f, "binary({})", len),
      Ty::Clob                    => write!(f, "clob"),
      Ty::Blob                    => write!(f, "blob"),
      Ty::Array(ref ty)           => write!(f, "array<{}>", ty),
      Ty::Struct(ref tys)         => {
        write!(f, "struct({})", tys.iter().map(|f| format!("{}", f)).join(", "))
      },        
      Ty::Map (ref kty, ref vty)  => write!(f, "map<{},{}>", kty, vty)
    }
  }
}

#[allow(non_camel_case_types)]
pub type BOOL      = bool;
#[allow(non_camel_case_types)]
pub type INT1      = i8;
#[allow(non_camel_case_types)]
pub type INT2      = i16;
#[allow(non_camel_case_types)]
pub type INT4      = i32;
#[allow(non_camel_case_types)]
pub type INT8      = i64;
#[allow(non_camel_case_types)]
pub type FLOAT4    = f32;
#[allow(non_camel_case_types)]
pub type FLOAT8    = f64;
#[allow(non_camel_case_types)]
pub type DATE      = i32;
#[allow(non_camel_case_types)]
pub type TIME      = i64;
#[allow(non_camel_case_types)]
pub type TIMESTAMP = i64;
#[allow(non_camel_case_types)]
pub type TEXT      = StrSlice;  


/// Determine a result data type from two expression data types.
#[allow(unused_variables)]
pub fn result_data_ty(lhs_ty: &Ty, rhs_ty: &Ty) -> Ty {
  match *lhs_ty {
    Ty::Bool => {
      match *rhs_ty {
        Ty::Bool => rhs_ty.clone(),
        _ => panic!("Undefined Operator")
      }
    },


    Ty::Int1 => {
      match *rhs_ty {
        Ty::Int1   |
        Ty::Int2   |
        Ty::Int4   |
        Ty::Int8   |
        Ty::Float4 |
        Ty::Float8 => rhs_ty.clone(),
        _ => panic!("Undefined Operator")
      }
    },

    Ty::Int2 => {
      match *rhs_ty {
        Ty::Int2   |
        Ty::Int4   |
        Ty::Int8   |
        Ty::Float4 |
        Ty::Float8 => rhs_ty.clone(),

        Ty::Int1   => lhs_ty.clone(),
        _ => panic!("Undefined Operator")
      }
    },

    Ty::Int4 => {
      match *rhs_ty {
        Ty::Int4   |
        Ty::Int8   |
        Ty::Float4 |
        Ty::Float8 => rhs_ty.clone(),

        Ty::Int1 |
        Ty::Int2 => lhs_ty.clone(),

        _ => panic!("Undefined Operator")
      }
    },

    Ty::Int8 => {
      match *rhs_ty {
        Ty::Int8   |
        Ty::Float4 |
        Ty::Float8 => rhs_ty.clone(),

        Ty::Int1 |
        Ty::Int2 |
        Ty::Int4 => lhs_ty.clone(),

        _ => panic!("Undefined Operator")
      }
    },

    Ty::Float4 => {
      match *rhs_ty {
        Ty::Float4 |
        Ty::Float8 => rhs_ty.clone(),

        Ty::Int1 |
        Ty::Int2 |
        Ty::Int4 |
        Ty::Int8 => lhs_ty.clone(),

        _ => panic!("Undefined Operator")
      }
    },

    Ty::Float8 => {
      match *rhs_ty {
        Ty::Float8 => rhs_ty.clone(),

        Ty::Int1   |
        Ty::Int2   |
        Ty::Int4   |
        Ty::Int8   |
        Ty::Float4 => lhs_ty.clone(),

        _ => panic!("Undefined Operator")
      }
    },
    
    Ty::Numeric(ref p, ref s) => {
      panic!("Undefined operator")
    },

    Ty::Date => {
      panic!("Undefined Operator")
    },
    
    Ty::Time => {
      panic!("Undefined Operator")
    },
    
    Ty::Timez => {
      panic!("Undefined Operator")
    },

    Ty::Timestamp => {
      panic!("Undefined Operator")
    },
    
    Ty::Timestampz => {
      panic!("Undefined Operator")
    },

    Ty::Interval => {
      panic!("Undefined Operator")
    },

    Ty::Char(ref len) => {
      panic!("Undefined Operator")
    },
    
    Ty::Binary(ref len) => {
      panic!("Undefined Operator")
    },

    Ty::Clob => {
      match *rhs_ty {
        Ty::Clob => rhs_ty.clone(),
        _ => panic!("Undefined Operator")
      }
    },
    
    Ty::Blob => {
      match *rhs_ty {
        Ty::Blob => rhs_ty.clone(),
        _ => panic!("Undefined Operator")
      }
    },
    
    Ty::Array(ref ty) => {
      panic!("Undefined Operator")
    }
    
    Ty::Struct(ref tys) => {
      panic!("Undefined Operator")
    },
    
    Ty::Map(ref kty, ref vty) => {
      panic!("Undefined Operator")
    }
  }
}
