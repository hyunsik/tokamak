use std::fmt;
use std::mem;

use common::err::TResult;
use util::str::StrSlice;

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum TyKind {
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

pub trait HasTy {
  fn data_ty(&self) -> &Ty;
}

pub const BOOL_TY     : &'static Ty = &Ty::new(TyKind::Bool);
pub const INT1_TY     : &'static Ty = &Ty::new(TyKind::Int1);
pub const INT2_TY     : &'static Ty = &Ty::new(TyKind::Int2);
pub const INT4_TY     : &'static Ty = &Ty::new(TyKind::Int4);
pub const INT8_TY     : &'static Ty = &Ty::new(TyKind::Int8);
pub const FLOAT4_TY   : &'static Ty = &Ty::new(TyKind::Float4);
pub const FLOAT8_TY   : &'static Ty = &Ty::new(TyKind::Float8);
pub const DATE_TY     : &'static Ty = &Ty::new(TyKind::Date);
pub const TIME_TY     : &'static Ty = &Ty::new(TyKind::Time);
pub const TIMESTAMP_TY: &'static Ty = &Ty::new(TyKind::Timestamp);
pub const INTERVAL_TY : &'static Ty = &Ty::new(TyKind::Interval);
pub const TEXT_TY     : &'static Ty = &Ty::new(TyKind::Text);
pub const CHAR_TY     : &'static Ty = &Ty::new_vartype(TyKind::Char, 255);

pub const BOOL_STR     : &'static str = "bool";
pub const INT1_STR     : &'static str = "int1";
pub const INT2_STR     : &'static str = "int2";
pub const INT4_STR     : &'static str = "int4";
pub const INT8_STR     : &'static str = "int8";
pub const FLOAT4_STR   : &'static str = "float4";
pub const FLOAT8_STR   : &'static str = "float8";
pub const DATE_STR     : &'static str = "date";
pub const TIME_STR     : &'static str = "time";
pub const TIMESTAMP_STR: &'static str = "timestamp";
pub const INTERVAL_STR : &'static str = "interval";
pub const TEXT_STR     : &'static str = "text";
pub const CHAR_STR     : &'static str = "char";

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

/// Data Domain for each field
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Ty {
  pub kind : TyKind,
  pub len : u32, // for CHAR, VARCHAR
  pub precision : u8, // for numeric or decimal
  pub scale : u8 // for numeric or decimal
}

impl Ty {
  pub const fn new (kind : TyKind) -> Ty {
    Ty {kind: kind, len : 0, precision: 0, scale: 0}
  }

  pub const fn new_vartype(kind : TyKind, len: u32) -> Ty {
    Ty {kind: kind, len : len, precision: 0, scale: 0}
  }

  pub fn kind(&self) -> TyKind {
    self.kind
  }

  pub fn bytes_len(&self) -> u32 {
    Ty::size_of(self)
  }

  #[inline(always)]
  pub fn size_of(data_type: &Ty) -> u32 {
    match data_type.kind {
      TyKind::Bool      => 1,
      TyKind::Int1      => 1,
      TyKind::Int2      => 2,
      TyKind::Int4      => 4,
      TyKind::Int8      => 8,
      TyKind::Float4    => 4,
      TyKind::Float8    => 8,
      TyKind::Date      => 4,
      TyKind::Time      => 8,
      TyKind::Timestamp => 8,
      TyKind::Interval  => 12,
      TyKind::Char      => data_type.len,
      TyKind::Text      => mem::size_of::<TEXT>() as u32,
      TyKind::Varchar | TyKind::Blob => 12,
    }
  }

  pub fn has_length(data_type: &Ty) -> bool {
    match data_type.kind {
      TyKind::Char | TyKind::Varchar | TyKind::Blob => true,
      _ => false
    }
  }

  pub fn is_variable(data_type: &Ty) -> bool {
    match data_type.kind {
      TyKind::Varchar | TyKind::Blob => true,
      _ => false
    }
  }

  pub fn name(&self) -> &'static str {
    match self.kind {
      TyKind::Bool      => BOOL_STR,
      TyKind::Int1      => INT1_STR,
      TyKind::Int2      => INT2_STR,
      TyKind::Int4      => INT4_STR,
      TyKind::Int8      => INT8_STR,
      TyKind::Float4    => FLOAT4_STR,
      TyKind::Float8    => FLOAT8_STR,
      TyKind::Time      => TIME_STR,
      TyKind::Date      => DATE_STR,
      TyKind::Timestamp => TIMESTAMP_STR,
      TyKind::Interval  => INTERVAL_STR,
      TyKind::Text      => TEXT_STR,
      TyKind::Char      => CHAR_STR,
      _                 => panic!("Unsupported type")
    }
  }
}

impl HasTy for Ty {
  #[inline]
  fn data_ty(&self) -> &Ty {
    &self
  }
}

impl fmt::Display for Ty {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.name())
  }
}

/// Determine a result data type from two expression data types.
pub fn result_data_ty(&lhs_ty: &Ty, &rhs_ty: &Ty) -> Ty {
  match lhs_ty.kind() {

    TyKind::Bool => {
      match rhs_ty.kind() {
        TyKind::Bool => rhs_ty.clone(),
        _ => panic!("Undefined Operator")
      }
    },


    TyKind::Int1 => {
      match rhs_ty.kind() {
        TyKind::Int1   |
        TyKind::Int2   |
        TyKind::Int4   |
        TyKind::Int8   |
        TyKind::Float4 |
        TyKind::Float8 => rhs_ty.clone(),
        _ => panic!("Undefined Operator")
      }
    },

    TyKind::Int2 => {
      match rhs_ty.kind() {
        TyKind::Int2   |
        TyKind::Int4   |
        TyKind::Int8   |
        TyKind::Float4 |
        TyKind::Float8 => rhs_ty.clone(),

        TyKind::Int1   => lhs_ty.clone(),
        _ => panic!("Undefined Operator")
      }
    },

    TyKind::Int4 => {
      match rhs_ty.kind() {
        TyKind::Int4   |
        TyKind::Int8   |
        TyKind::Float4 |
        TyKind::Float8 => rhs_ty.clone(),

        TyKind::Int1 |
        TyKind::Int2 => lhs_ty.clone(),

        _ => panic!("Undefined Operator")
      }
    },

    TyKind::Int8 => {
      match rhs_ty.kind() {
        TyKind::Int8   |
        TyKind::Float4 |
        TyKind::Float8 => rhs_ty.clone(),

        TyKind::Int1 |
        TyKind::Int2 |
        TyKind::Int4 => lhs_ty.clone(),

        _ => panic!("Undefined Operator")
      }
    },

    TyKind::Float4 => {
      match rhs_ty.kind() {
        TyKind::Float4 |
        TyKind::Float8 => rhs_ty.clone(),

        TyKind::Int1 |
        TyKind::Int2 |
        TyKind::Int4 |
        TyKind::Int8 => lhs_ty.clone(),

        _ => panic!("Undefined Operator")
      }
    },

    TyKind::Float8 => {
      match rhs_ty.kind() {
        TyKind::Float8 => rhs_ty.clone(),

        TyKind::Int1   |
        TyKind::Int2   |
        TyKind::Int4   |
        TyKind::Int8   |
        TyKind::Float4 => lhs_ty.clone(),

        _ => panic!("Undefined Operator")
      }
    },

    TyKind::Time => {
      panic!("Undefined Operator")
    },

    TyKind::Date => {
      panic!("Undefined Operator")
    },

    TyKind::Timestamp => {
      panic!("Undefined Operator")
    },

    TyKind::Interval => {
      panic!("Undefined Operator")
    },

    TyKind::Char | TyKind::Varchar => {
      panic!("Undefined Operator")
    },

    TyKind::Text => {
      match rhs_ty.kind() {
        TyKind::Text => rhs_ty.clone(),

        _ => panic!("Undefined Operator")
      }
    },

    TyKind::Blob => {
      panic!("Undefined Operator")
    }
  }
}
