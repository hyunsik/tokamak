use std::mem;
use common::err::TResult;
use common::string_slice::StringSlice;

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Ty {
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

pub trait HasDataTy {
  fn data_ty(&self) -> &DataTy;
}

pub const BOOL_TY     : &'static DataTy = &DataTy::new(Ty::Bool);
pub const INT1_TY     : &'static DataTy = &DataTy::new(Ty::Int1);
pub const INT2_TY     : &'static DataTy = &DataTy::new(Ty::Int2);
pub const INT4_TY     : &'static DataTy = &DataTy::new(Ty::Int4);
pub const INT8_TY     : &'static DataTy = &DataTy::new(Ty::Int8);
pub const FLOAT4_TY   : &'static DataTy = &DataTy::new(Ty::Float4);
pub const FLOAT8_TY   : &'static DataTy = &DataTy::new(Ty::Float8);
pub const DATE_TY     : &'static DataTy = &DataTy::new(Ty::Date);
pub const TIME_TY     : &'static DataTy = &DataTy::new(Ty::Time);
pub const TIMESTAMP_TY: &'static DataTy = &DataTy::new(Ty::Timestamp);
pub const TEXT_TY     : &'static DataTy = &DataTy::new(Ty::Text);
pub const INTERVAL_TY : &'static DataTy = &DataTy::new(Ty::Interval);
pub const CHAR_TY     : &'static DataTy = &DataTy::new_vartype(Ty::Char, 255);

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
pub type TEXT      = StringSlice;

/// Data Domain for each field
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct DataTy {
  pub kind : Ty, 
  pub len : u32, // for CHAR, VARCHAR
  pub precision : u8, // for numeric or decimal
  pub scale : u8 // for numeric or decimal
}

impl DataTy {
  pub const fn new (kind : Ty) -> DataTy {
    DataTy {kind: kind, len : 0, precision: 0, scale: 0}
  }

  pub const fn new_vartype(kind : Ty, len: u32) -> DataTy {
    DataTy {kind: kind, len : len, precision: 0, scale: 0}
  }

  pub fn kind(&self) -> Ty {
    self.kind
  }

  pub fn bytes_len(&self) -> u32 {
    DataTy::size_of(self)
  }

  #[inline(always)]
  pub fn size_of(data_type: &DataTy) -> u32 {
    match data_type.kind {
      Ty::Bool => 1,        
      Ty::Int1 => 1,
      Ty::Int2 => 2,
      Ty::Int4 => 4,
      Ty::Int8 => 8,
      Ty::Float4 => 4,
      Ty::Float8 => 8,
      Ty::Date => 4,
      Ty::Time => 8,
      Ty::Timestamp => 8,
      Ty::Interval => 12,
      Ty::Char => data_type.len,
      Ty::Text => mem::size_of::<TEXT>() as u32,
      Ty::Varchar | Ty::Blob => 12,
    }
  }

  pub fn has_length(data_type: &DataTy) -> bool {
    match data_type.kind {
      Ty::Char | Ty::Varchar | Ty::Blob => true,
      _ => false
    }
  }

  pub fn is_variable(data_type: &DataTy) -> bool {
    match data_type.kind {
      Ty::Varchar | Ty::Blob => true,
      _ => false
    }
  }
}

impl HasDataTy for DataTy {
  #[inline]
  fn data_ty(&self) -> &DataTy {
    &self
  }
}

/// Determine a result data type from two expression data types.
pub fn result_data_ty(&lhs_ty: &DataTy, &rhs_ty: &DataTy) -> DataTy {
  match lhs_ty.kind() {
    
    Ty::Bool => {
      match rhs_ty.kind() {
        Ty::Bool => rhs_ty.clone(),
        _ => panic!("Undefined Operator")
      }
    },


    Ty::Int1 => {
      match rhs_ty.kind() {
        Ty::Int1 | Ty::Int2 | Ty::Int4 | Ty::Int8 | Ty::Float4 | Ty::Float8 =>{
          rhs_ty.clone()
        },
        _ => panic!("Undefined Operator")
      }
    },

    Ty::Int2 => {
      match rhs_ty.kind() {
        Ty::Int2 | Ty::Int4 | Ty::Int8 | Ty::Float4 | Ty::Float8 => {
          rhs_ty.clone()
        },
        Ty::Int1 => lhs_ty.clone(),
        _ => panic!("Undefined Operator")
      }
    },

    Ty::Int4 => {
      match rhs_ty.kind() {
        Ty::Int4 | Ty::Int8 | Ty::Float4 | Ty::Float8 => rhs_ty.clone(),
        Ty::Int1 | Ty::Int2 => lhs_ty.clone(),
        _ => panic!("Undefined Operator")
      }
    },

    Ty::Int8 => {
      match rhs_ty.kind() {
        Ty::Int8 | Ty::Float4 | Ty::Float8 => rhs_ty.clone(),
        Ty::Int1 | Ty::Int2 | Ty::Int4 => lhs_ty.clone(),
        _ => panic!("Undefined Operator")
      }
    },

    Ty::Float4 => {
      match rhs_ty.kind() {
        Ty::Float4 | Ty::Float8 => rhs_ty.clone(),
        Ty::Int1 | Ty::Int2 | Ty::Int4 | Ty::Int8 => lhs_ty.clone(),
        _ => panic!("Undefined Operator")
      }
    },

    Ty::Float8 => {
      match rhs_ty.kind() {
        Ty::Float8 => rhs_ty.clone(),
        Ty::Int1 | Ty::Int2 | Ty::Int4 | Ty::Int8 | Ty::Float4 => lhs_ty.clone(),
        _ => panic!("Undefined Operator")
      }
    },

    Ty::Time => {      
      panic!("Undefined Operator")
    },

    Ty::Date => {
      panic!("Undefined Operator")
    },

    Ty::Timestamp => {
      panic!("Undefined Operator")
    },

    Ty::Interval => {
      panic!("Undefined Operator")
    },

    Ty::Char | Ty::Varchar => {
      panic!("Undefined Operator")
    },

    Ty::Text => {
      match rhs_ty.kind() {
        Ty::Text => rhs_ty.clone(),
        _ => panic!("Undefined Operator")
      }
    },

    Ty::Blob => {
      panic!("Undefined Operator")
    }
  } 
}