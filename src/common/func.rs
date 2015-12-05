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
pub type UnaryFn   = Rc<Fn(&mut MiniPageWriter, &MiniPage, Option<u32>, usize) -> Void>;
pub type BinaryFn  = Rc<Fn(&MiniPage, &MiniPage, &mut MiniPageWriter, usize) -> Void>;
pub type TrinityFn = Rc<Fn(&MiniPage, &MiniPage, &MiniPage, &mut MiniPageWriter, usize) -> Void>;

#[derive(Clone)]
pub struct Function 
{
  ret_ty  : Ty,      // return type
  arg_tys : Vec<Ty>, // argument data types  
  kind    : FnKind,  
  method  : InvokeMethod
}


impl Function
{
	pub fn new(ret_ty: Ty, arg_tys: Vec<Ty>, kind: FnKind, method: InvokeMethod) -> Function 
	{
		Function {
			ret_ty: ret_ty,
      arg_tys : arg_tys,
			kind : kind,
			method  : method
		}
	}
}

#[derive(Eq, Copy, Clone, PartialEq, PartialOrd, Ord)]
pub enum FnKind 
{
  Scalar,
  Aggregation,
  Window
}

#[derive(Clone)]
pub enum InvokeMethod
{
  NoArgOp   (NoArgFn),
  UnaryOp   (UnaryFn),
  BinaryOp  (BinaryFn),
  TrinityOp (TrinityFn)
}