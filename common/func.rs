//! Function System
//!
//! In Tokamak, all arithmetic operators, predicates, and functions are regarded as
//! a kind of functions. For example, the arithmetic operator plus (+) for i32 values 
//! can be represented as a triple ("+", [i32, i32], FuncKind::Scalar).
//!

use std::cmp::Ordering;
use std::rc::Rc;
use std::cell::RefCell;

use err::{Result, Void, void_ok};
use plugin::{PluginManager, TypeRegistry};
use rows::{MiniPage,MiniPageWriter};
use types::Ty;

pub type NoArgFn   = Rc<Fn(&mut MiniPageWriter, usize) -> Void>;
pub type UnaryFn   = Rc<Fn(&MiniPage, &mut MiniPageWriter, usize) -> Void>;
pub type BinaryFn  = Rc<Fn(&MiniPage, &MiniPage, &mut MiniPageWriter, usize) -> Void>;
pub type TrinityFn = Rc<Fn(&MiniPage, &MiniPage, &MiniPage, &mut MiniPageWriter, usize) -> Void>;

#[derive(Eq, Copy, Clone, PartialEq, PartialOrd, Ord)]
pub enum FnKind 
{
  Scalar,
  Aggregation,
  Window
}

#[derive(Clone)]
pub struct FuncBody 
{
  pub ret_type   : Ty,
  pub kind    : FnKind,
  pub method     : InvokeMethod
}

impl FuncBody
{
	pub fn new(ret_type: Ty, kind: FnKind, method: InvokeMethod) -> FuncBody
	{
		FuncBody {
			ret_type: ret_type,
			kind : kind,
			method  : method
		}
	}
}

#[derive(Clone)]
pub enum InvokeMethod
{
  NoArgOp   (NoArgFn),
  UnaryOp   (UnaryFn),
  BinaryOp  (BinaryFn),
  TrinityOp (TrinityFn)
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct FuncSignature 
{
  // Function Name
  name     : String,
  // Function argument data types
  arg_types: Vec<Ty>,
  // Function kind
  fn_kind  : FnKind
}

impl FuncSignature
{
  pub fn new(name: String, arg_types: Vec<Ty>, fn_kind: FnKind) -> FuncSignature
  {
    FuncSignature {
      name     : name,
      arg_types: arg_types,
      fn_kind  : fn_kind
    }
  }
}