use std::mem;
use common::err::{Error, TResult};
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

pub trait HasTy {
  fn ty(&self) -> Ty;
}

pub trait HasDataTy {
  fn data_ty(&self) -> &DataType;
}

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
pub type TEXT_T      = StringSlice;

/// Data Domain for each field
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct DataType {
  pub ty : Ty, 
  pub len : u32, // for CHAR, VARCHAR
  pub precision : u8, // for numeric or decimal
  pub scale : u8 // for numeric or decimal
}

impl DataType {
  pub fn new (ty : Ty) -> DataType {
    return DataType {ty: ty, len : 0, precision: 0, scale: 0};
  }

  pub fn new_vartype(ty : Ty, len: u32) -> DataType {
    return DataType {ty: ty, len : len, precision: 0, scale: 0};
  }

  pub fn bytes_len(&self) -> u32 {
    DataType::size_of(self)
  }

  #[inline(always)]
  pub fn size_of(data_type: &DataType) -> u32 {
    match data_type.ty {
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
      Ty::Text => mem::size_of::<TEXT_T>()as u32,
      Ty::Varchar | Ty::Blob => 12,
    }
  }

  pub fn has_length(data_type: &DataType) -> bool {
    match data_type.ty {
      Ty::Char | Ty::Varchar | Ty::Blob => true,
      _ => false
    }
  }

  pub fn is_variable(data_type: &DataType) -> bool {
    match data_type.ty {
      Ty::Varchar | Ty::Blob => true,
      _ => false
    }
  }
}

impl HasDataTy for DataType {
  #[inline]
  fn data_ty(&self) -> &DataType {
    &self
  }
}

impl HasTy for DataType {
  #[inline]
  fn ty(&self) -> Ty {
   self.ty
  }
}

/// Determine a result data type from two expression data types.
pub fn result_data_ty(&lhs_ty: &DataType, &rhs_ty: &DataType) -> DataType {
  match lhs_ty.ty() {
    
    Ty::Bool => {
      match rhs_ty.ty() {
        Ty::Bool => rhs_ty.clone(),
        _ => panic!("Undefined Operator")
      }
    },


    Ty::Int1 => {
      match rhs_ty.ty() {
        Ty::Int1 | Ty::Int2 | Ty::Int4 | Ty::Int8 | Ty::Float4 | Ty::Float8 =>{
          rhs_ty.clone()
        },
        _ => panic!("Undefined Operator")
      }
    },

    Ty::Int2 => {
      match rhs_ty.ty() {
        Ty::Int2 | Ty::Int4 | Ty::Int8 | Ty::Float4 | Ty::Float8 => {
          rhs_ty.clone()
        },
        Ty::Int1 => lhs_ty.clone(),
        _ => panic!("Undefined Operator")
      }
    },

    Ty::Int4 => {
      match rhs_ty.ty() {
        Ty::Int4 | Ty::Int8 | Ty::Float4 | Ty::Float8 => rhs_ty.clone(),
        Ty::Int1 | Ty::Int2 => lhs_ty.clone(),
        _ => panic!("Undefined Operator")
      }
    },

    Ty::Int8 => {
      match rhs_ty.ty() {
        Ty::Int8 | Ty::Float4 | Ty::Float8 => rhs_ty.clone(),
        Ty::Int1 | Ty::Int2 | Ty::Int4 => lhs_ty.clone(),
        _ => panic!("Undefined Operator")
      }
    },

    Ty::Float4 => {
      match rhs_ty.ty() {
        Ty::Float4 | Ty::Float8 => rhs_ty.clone(),
        Ty::Int1 | Ty::Int2 | Ty::Int4 | Ty::Int8 => lhs_ty.clone(),
        _ => panic!("Undefined Operator")
      }
    },

    Ty::Float8 => {
      match rhs_ty.ty() {
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
      match rhs_ty.ty() {
        Ty::Text => rhs_ty.clone(),
        _ => panic!("Undefined Operator")
      }
    },

    Ty::Blob => {
      panic!("Undefined Operator")
    }
  } 
}