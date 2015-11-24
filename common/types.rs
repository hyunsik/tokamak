//! Type represents a data type. Its implementation is pluggable. 
//! So, Types are loaded from the packages.
//!

use std::mem;
use std::cmp::Ordering;
use std::rc::Rc;

use err::Result;
use rows::{MiniPage, FMiniPage};

pub type TypeFactory = Rc<Fn(&str) -> Result<Ty>>;

#[derive(Clone)]
pub struct TypeHandler {
  pub create_minipage: Rc<Fn() -> Box<MiniPage>>
}

#[derive(Clone, Eq, PartialEq, PartialOrd, Ord)]
pub struct Ty {
  base         : String,
  comparable   : bool,
  orderable    : bool,
  handler      : TypeHandler
}

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
}

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


/*

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

pub const BOOL_STR      : &'static str = "bool";
pub const I8_STR        : &'static str = "i32";
pub const I32_STR       : &'static str = "i32";
pub const I64_STR       : &'static str = "i64";
pub const F32_STR       : &'static str = "f32";
pub const F64_STR       : &'static str = "f64";

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