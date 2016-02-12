//! Type represents a data type. Its implementation is pluggable.
//! So, Types are loaded from the packages.
//!

use std::fmt;
use std::rc::Rc;

use itertools::Itertools;

use err::Result;

pub type TypeFactory = Rc<Fn(&str) -> Result<Ty>>;

pub mod name {
  pub static BOOL: &'static str = "bool";
  pub static U8: &'static str = "u8";
  pub static I8: &'static str = "i8";
  pub static I16: &'static str = "i16";
  pub static I32: &'static str = "i32";
  pub static I64: &'static str = "i64";
  pub static F32: &'static str = "f32";
  pub static F64: &'static str = "f64";
  pub static ARRAY: &'static str = "Array";
  pub static TUPLE: &'static str = "Tuple";
}

pub static BOOL: &'static Ty = &Ty::Bool;
pub static U8: &'static Ty = &Ty::U8;
pub static I8: &'static Ty = &Ty::I8;
pub static I16: &'static Ty = &Ty::I16;
pub static I32: &'static Ty = &Ty::I32;
pub static I64: &'static Ty = &Ty::I64;
pub static F32: &'static Ty = &Ty::F32;
pub static F64: &'static Ty = &Ty::F64;

/// Types
///
/// * Bool
/// * U8 : unsigned 8 bit integer
/// * I8 : signed 8 bit integer
/// * I16: signed 16 bit integer
/// * I32: signed 32 bit integer
/// * I64: signed 64 bit integer
/// * F32: single precision floating type
/// * F64: double precision floating type
/// * Array: multi-dimensional array of same elements
/// * Tuple: an ordered list of arbtrary type values
#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Debug)]
pub enum Ty {
  Bool,
  U8,
  I8,
  I16,
  I32,
  I64,
  F32,
  F64,
  Array(Box<Ty>, Vec<usize>),
  Tuple(Vec<Ty>),
}

impl Ty {
  /// Get a base name
  pub fn base(&self) -> &str {
    match *self {
      Ty::Bool => name::BOOL,
      Ty::U8 => name::U8,
      Ty::I8 => name::I8,
      Ty::I16 => name::I16,
      Ty::I32 => name::I32,
      Ty::I64 => name::I64,
      Ty::F32 => name::F32,
      Ty::F64 => name::F64,
      Ty::Array(..) => panic!("unsupported type: vector"),
      Ty::Tuple(_) => panic!("unsupported type: tuple"),
    }
  }

  #[inline(always)]
  pub fn is_int(&self) -> bool {
    self.is_sint() || self.is_uint()
  }

  pub fn is_sint(&self) -> bool {
    match *self {
      Ty::Bool |
      Ty::I8 |
      Ty::I16 |
      Ty::I32 |
      Ty::I64 => true,
      _ => false,
    }
  }

  pub fn is_uint(&self) -> bool {
    *self == Ty::U8
  }

  pub fn is_float(&self) -> bool {
    *self == Ty::F32 || *self == Ty::F64
  }

  pub fn size_of(&self) -> usize {
    match *self {
      Ty::Bool => 1,
      Ty::U8 | Ty::I8 => 1,
      Ty::I16 => 2,
      Ty::I32 | Ty::F32 => 4,
      Ty::I64 | Ty::F64 => 8,
      Ty::Array(ref ty, ref dims) => dims.iter().fold(0, |acc, dim| acc + (ty.size_of() * dim)),
      Ty::Tuple(ref tys) => tys.iter().fold(0, |acc, ty| acc + ty.size_of()),
    }
  }

  pub fn is_variable(&self) -> bool {
    false
  }
}

pub fn u(size: u32) -> &'static Ty {
  match size {
    8 => U8,
    _ => panic!("unsupported integer type: i{}", size),
  }
}

pub fn i(size: u32) -> &'static Ty {
  match size {
    8 => I8,
    16 => I16,
    32 => I32,
    64 => I64,
    _ => panic!("unsupported integer type: i{}", size),
  }
}

pub fn f(size: u32) -> &'static Ty {
  match size {
    32 => F32,
    64 => F64,
    _ => panic!("unsupported integer type: f{}", size),
  }
}

#[allow(non_snake_case)]
pub fn Array(ty: &Ty, dims: &[usize]) -> Ty {
  Ty::Array(Box::new(ty.clone()), dims.to_vec())
}

#[allow(non_snake_case)]
pub fn Tuple(types: &[&Ty]) -> Ty {
  Ty::Tuple(types.iter().map(|ty| (*ty).clone()).collect::<Vec<Ty>>())
}

/// Typed implementation
pub trait HasType {
  fn ty(&self) -> &Ty;
}


impl fmt::Display for Ty {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match *self {
      Ty::Bool => write!(f, "bool"),
      Ty::I8 => write!(f, "i8"),
      Ty::U8 => write!(f, "u8"),
      Ty::I16 => write!(f, "i16"),
      Ty::I32 => write!(f, "i32"),
      Ty::I64 => write!(f, "i64"),
      Ty::F32 => write!(f, "f32"),
      Ty::F64 => write!(f, "f64"),
      Ty::Array(ref ty, ref dims) => {
        write!(f,
               "Array({},{})",
               ty,
               dims.iter().map(|dim| format!("{}", dim)).join(","))
      }
      Ty::Tuple(ref types) => {
        write!(f,
               "({})",
               types.iter().map(|dim| format!("{}", dim)).join(","))
      }
    }
  }
}

// Determine a result data type from two expression data types.
// #[allow(unused_variables)]
// pub fn result_data_ty(lhs_ty: &Ty, rhs_ty: &Ty) -> Ty {
// match *lhs_ty {
// Ty::Bool => {
// match *rhs_ty {
// Ty::Bool => rhs_ty.clone(),
// _ => panic!("Undefined Operator")
// }
// },
//
//
// Ty::Int1 => {
// match *rhs_ty {
// Ty::Int1   |
// Ty::Int2   |
// Ty::Int4   |
// Ty::Int8   |
// Ty::Float4 |
// Ty::Float8 => rhs_ty.clone(),
// _ => panic!("Undefined Operator")
// }
// },
//
// Ty::Int2 => {
// match *rhs_ty {
// Ty::Int2   |
// Ty::Int4   |
// Ty::Int8   |
// Ty::Float4 |
// Ty::Float8 => rhs_ty.clone(),
//
// Ty::Int1   => lhs_ty.clone(),
// _ => panic!("Undefined Operator")
// }
// },
//
// Ty::Int4 => {
// match *rhs_ty {
// Ty::Int4   |
// Ty::Int8   |
// Ty::Float4 |
// Ty::Float8 => rhs_ty.clone(),
//
// Ty::Int1 |
// Ty::Int2 => lhs_ty.clone(),
//
// _ => panic!("Undefined Operator")
// }
// },
//
// Ty::Int8 => {
// match *rhs_ty {
// Ty::Int8   |
// Ty::Float4 |
// Ty::Float8 => rhs_ty.clone(),
//
// Ty::Int1 |
// Ty::Int2 |
// Ty::Int4 => lhs_ty.clone(),
//
// _ => panic!("Undefined Operator")
// }
// },
//
// Ty::Float4 => {
// match *rhs_ty {
// Ty::Float4 |
// Ty::Float8 => rhs_ty.clone(),
//
// Ty::Int1 |
// Ty::Int2 |
// Ty::Int4 |
// Ty::Int8 => lhs_ty.clone(),
//
// _ => panic!("Undefined Operator")
// }
// },
//
// Ty::Float8 => {
// match *rhs_ty {
// Ty::Float8 => rhs_ty.clone(),
//
// Ty::Int1   |
// Ty::Int2   |
// Ty::Int4   |
// Ty::Int8   |
// Ty::Float4 => lhs_ty.clone(),
//
// _ => panic!("Undefined Operator")
// }
// },
//
// Ty::Numeric(ref p, ref s) => {
// panic!("Undefined operator")
// },
//
// Ty::Date => {
// panic!("Undefined Operator")
// },
//
// Ty::Time => {
// panic!("Undefined Operator")
// },
//
// Ty::Timez => {
// panic!("Undefined Operator")
// },
//
// Ty::Timestamp => {
// panic!("Undefined Operator")
// },
//
// Ty::Timestampz => {
// panic!("Undefined Operator")
// },
//
// Ty::Interval => {
// panic!("Undefined Operator")
// },
//
// Ty::Char(ref len) => {
// panic!("Undefined Operator")
// },
//
// Ty::Binary(ref len) => {
// panic!("Undefined Operator")
// },
//
// Ty::Clob => {
// match *rhs_ty {
// Ty::Clob => rhs_ty.clone(),
// _ => panic!("Undefined Operator")
// }
// },
//
// Ty::Blob => {
// match *rhs_ty {
// Ty::Blob => rhs_ty.clone(),
// _ => panic!("Undefined Operator")
// }
// },
//
// Ty::Array(ref ty) => {
// panic!("Undefined Operator")
// }
//
// Ty::Struct(ref tys) => {
// panic!("Undefined Operator")
// },
//
// Ty::Map(ref kty, ref vty) => {
// panic!("Undefined Operator")
// }
// }
// }
//

// pub fn bool_ty() -> Ty {
// let handler = TypeHandler {
// create_minipage: Rc::new(|| -> Box<MiniPage> {
// Box::new(FMiniPage::new(mem::size_of::<bool>()))
// })
// };
//
// Ty::new(BOOL_STR, true, true, handler)
// }
//
// pub fn i8_ty() -> Ty {
// let handler = TypeHandler {
// create_minipage: Rc::new(|| -> Box<MiniPage> {
// Box::new(FMiniPage::new(mem::size_of::<i8>()))
// })
// };
//
// Ty::new(I8_STR, true, true, handler)
// }
//
// pub fn i16_ty() -> Ty {
// let handler = TypeHandler {
// create_minipage: Rc::new(|| -> Box<MiniPage> {
// Box::new(FMiniPage::new(mem::size_of::<i16>()))
// })
// };
//
// Ty::new(I32_STR, true, true, handler)
// }
//
// pub fn i32_ty() -> Ty {
// let handler = TypeHandler {
// create_minipage: Rc::new(|| -> Box<MiniPage> {
// Box::new(FMiniPage::new(mem::size_of::<i32>()))
// })
// };
//
// Ty::new(I32_STR, true, true, handler)
// }
//
// pub fn i64_ty() -> Ty {
// let handler = TypeHandler {
// create_minipage: Rc::new(|| -> Box<MiniPage> {
// Box::new(FMiniPage::new(mem::size_of::<i64>()))
// })
// };
//
// Ty::new(I64_STR, true, true, handler)
// }
//
// pub fn f32_ty() -> Ty {
// let handler = TypeHandler {
// create_minipage: Rc::new(|| -> Box<MiniPage> {
// Box::new(FMiniPage::new(mem::size_of::<f32>()))
// })
// };
//
// Ty::new(F32_STR, true, true, handler)
// }
//
// pub fn f64_ty() -> Ty {
// let handler = TypeHandler {
// create_minipage: Rc::new(|| -> Box<MiniPage> {
// Box::new(FMiniPage::new(mem::size_of::<f64>()))
// })
// };
//
// Ty::new(F64_STR, true, true, handler)
// }
//

#[cfg(test)]
mod tests {
  use super::*;
  use libc::c_void;

  fn assert_display(ty: Ty, expected: &str) {
    assert_eq!(&format!("{}", ty), expected)
  }

  #[test]
  fn test_display_agg_types() {
    assert_display(Array(&Ty::F64, &[4, 4]), "Array(f64,4,4)");
    assert_display(Tuple(&[&Ty::F64, &Ty::F64, &Ty::I64]), "(f64,f64,i64)");
  }
}
