//! Type represents a data type. Its implementation is pluggable.
//! So, Types are loaded from the packages.
//!

use std::mem;
use std::cmp::Ordering;
use std::rc::Rc;

use err::Result;
use page::Chunk;

pub type TypeFactory = Rc<Fn(&str) -> Result<Ty>>;

#[derive(Clone)]
pub struct TypeHandler {
  pub create_minipage: Rc<Fn() -> Box<Chunk>>
}

/*
#[derive(Clone, Eq, PartialEq, PartialOrd, Ord)]
pub struct Ty {
  base         : String,
  comparable   : bool,
  orderable    : bool,
  handler      : TypeHandler
}
*/

/*
impl Ty {
	pub fn new(base: &str, comparable: bool, orderable: bool, handler: TypeHandler) -> Ty {
		Ty {
			base      : base.to_string(),
			comparable: comparable,
			orderable : orderable,
			handler   : handler
		}
	}

	pub fn base(&self) -> &str
	{
		&self.base
	}

	pub fn handler(&self) -> &TypeHandler
	{
		&self.handler
	}
}

/// For ignoring
impl Eq for TypeHandler {}

/// For ignoring
impl PartialEq for TypeHandler {
  fn eq(&self, other: &TypeHandler) -> bool {
    true
  }
}

/// For ignoring
impl PartialOrd for TypeHandler {
 fn partial_cmp(&self, other: &TypeHandler) -> Option<Ordering> {
   Some(Ordering::Equal)
 }
}

/// For ignoring
impl Ord for TypeHandler {
  fn cmp(&self, other: &TypeHandler) -> Ordering {
    Ordering::Equal
  }
}*/

//impl Ty {
//  pub fn kind(&self) -> &TyKind {
//    &self.kind
//  }
//
//  pub fn handler(&self) -> Rc<TypeHandler> {
//    match self.kind {
//      TyKind::I32 => {
//        let f = || -> Box<MiniPage> {Box::new(FMiniPage::new(mem::size_of::<f32>()))};
//        Rc::new(TypeHandler {create_minipage: Rc::new(f)})
//      },
//
//      TyKind::F32 => {
//        let f = || -> Box<MiniPage> {Box::new(FMiniPage::new(mem::size_of::<i32>()))};
//        Rc::new(TypeHandler {create_minipage: Rc::new(f)})
//      },
//
//      _  => { panic!("Unknown supported type") }
//    }
//  }
//}
//

pub mod name {
  pub static BOOL : &'static str = "bool";
  pub static U8   : &'static str = "u8";
  pub static I8   : &'static str = "i8";
  pub static I16  : &'static str = "i16";
  pub static I32  : &'static str = "i32";
  pub static I64  : &'static str = "i64";
  pub static F32  : &'static str = "f32";
  pub static F64  : &'static str = "f64";
}

pub static BOOL :&'static Ty = &Ty::Bool;
pub static U8   :&'static Ty = &Ty::U8;
pub static I8   :&'static Ty = &Ty::I8;
pub static I16  :&'static Ty = &Ty::I16;
pub static I32  :&'static Ty = &Ty::I32;
pub static I64  :&'static Ty = &Ty::I64;
pub static F32  :&'static Ty = &Ty::F32;
pub static F64  :&'static Ty = &Ty::F64;

#[macro_export]
macro_rules! schema {
  ( $( $t:expr ),* ) => {
    {
      let mut schema = Vec::new();
        $(
          schema.push($t.clone());
        )*

        schema
    }
  };
}

/// Ty
/// * Bool
/// * U8:
/// * I8:
/// * I16:
/// * I32:
/// * I64:
/// * F32: single precision floating type
/// * F64: double precision floating type
/// * Vector: an ordered list of same elements
/// * Tuple: an ordered list of arbtrary type values
#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Debug)]
pub enum Ty
{
  Bool,
  U8,
  I8,
  I16,
  I32,
  I64,
  F32,
  F64,
  Vector(Box<Ty>),
  Tuple (Vec<Ty>),
}

impl Ty
{
  /// Get a base name
  pub fn base(&self) -> &str
  {
    match *self {
      Ty::Bool => name::BOOL,
      Ty::U8   => name::U8,
      Ty::I8   => name::I8,
      Ty::I16  => name::I16,
      Ty::I32  => name::I32,
      Ty::I64  => name::I64,
      Ty::F32  => name::F32,
      Ty::F64  => name::F64,
      Ty::Vector(_) => panic!("unsupported type: vector"),
      Ty::Tuple (_) => panic!("unsupported type: tuple"),
    }
  }

  #[inline(always)]
  pub fn is_int(&self) -> bool
  {
    self.is_sint() || self.is_uint()
  }

  pub fn is_sint(&self) -> bool
  {
    match *self {
      Ty::Bool |
      Ty::I8 |
      Ty::I16 |
      Ty::I32 |
      Ty::I64 => true,
      _       => false
    }
  }

  pub fn is_uint(&self) -> bool
  {
    match *self {
      Ty::U8 => true,
      _      => false
    }
  }

  pub fn is_float(&self) -> bool
  {
    match *self {
      Ty::F32 | Ty::F64 => true,
      _                 => false
    }
  }

  pub fn size_of(&self) -> usize
  {
    match *self {
      Ty::Bool                  => 1,
      Ty::U8 | Ty::I8           => 1,
      Ty::I16                   => 2,
      Ty::I32 | Ty::F32         => 4,
      Ty::I64 | Ty::F64         => 8,
      Ty::Vector(_) => panic!("unsupported type: vector"),
      Ty::Tuple (_) => panic!("unsupported type: tuple"),
    }
  }

  pub fn is_variable(&self) -> bool
  {
    false
  }
}

pub fn u(size: u32) -> &'static Ty
{
  match size {
    8  => U8,
    _  => panic!("unsupported integer type: i{}", size)
  }
}

pub fn i(size: u32) -> &'static Ty
{
  match size {
    8  => I8,
    16 => I16,
    32 => I32,
    64 => I64,
    _  => panic!("unsupported integer type: i{}", size)
  }
}

pub fn f(size: u32) -> &'static Ty
{
  match size {
    32 => F32,
    64 => F64,
    _  => panic!("unsupported integer type: f{}", size)
  }
}

pub trait HasType {
  fn ty(&self) -> &Ty;
}

/*
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
*/

/*
pub fn bool_ty() -> Ty {
	let handler = TypeHandler {
	 	create_minipage: Rc::new(|| -> Box<MiniPage> {
			Box::new(FMiniPage::new(mem::size_of::<bool>()))
		})
  };

	Ty::new(BOOL_STR, true, true, handler)
}

pub fn i8_ty() -> Ty {
	let handler = TypeHandler {
	 	create_minipage: Rc::new(|| -> Box<MiniPage> {
			Box::new(FMiniPage::new(mem::size_of::<i8>()))
		})
  };

	Ty::new(I8_STR, true, true, handler)
}

pub fn i16_ty() -> Ty {
	let handler = TypeHandler {
	 	create_minipage: Rc::new(|| -> Box<MiniPage> {
			Box::new(FMiniPage::new(mem::size_of::<i16>()))
		})
  };

	Ty::new(I32_STR, true, true, handler)
}

pub fn i32_ty() -> Ty {
	let handler = TypeHandler {
	 	create_minipage: Rc::new(|| -> Box<MiniPage> {
			Box::new(FMiniPage::new(mem::size_of::<i32>()))
		})
  };

	Ty::new(I32_STR, true, true, handler)
}

pub fn i64_ty() -> Ty {
	let handler = TypeHandler {
	 	create_minipage: Rc::new(|| -> Box<MiniPage> {
			Box::new(FMiniPage::new(mem::size_of::<i64>()))
		})
  };

	Ty::new(I64_STR, true, true, handler)
}

pub fn f32_ty() -> Ty {
	let handler = TypeHandler {
	 	create_minipage: Rc::new(|| -> Box<MiniPage> {
			Box::new(FMiniPage::new(mem::size_of::<f32>()))
		})
  };

	Ty::new(F32_STR, true, true, handler)
}

pub fn f64_ty() -> Ty {
	let handler = TypeHandler {
	 	create_minipage: Rc::new(|| -> Box<MiniPage> {
			Box::new(FMiniPage::new(mem::size_of::<f64>()))
		})
  };

	Ty::new(F64_STR, true, true, handler)
}
*/
