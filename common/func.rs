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

#[derive(Eq, Copy, Clone, PartialEq, PartialOrd, Ord)]
pub enum FuncKind {
  Scalar,
  Aggregation,
  Window
}

pub type NoArgFn   = Rc<Fn(&mut MiniPageWriter, usize)>;
pub type UnaryFn   = Rc<Fn(&MiniPage, RefCell<MiniPageWriter>)>;
pub type BinaryFn  = Rc<Fn(&MiniPage, &MiniPage, RefCell<MiniPageWriter>)>;
pub type TrinityFn = Rc<Fn(&MiniPage, &MiniPage, &MiniPage, RefCell<MiniPageWriter>)>;

pub type ScalarVecFunc = Rc<Fn(Vec<&MiniPage>, RefCell<MiniPageWriter>)>;

#[derive(Clone)]
pub enum InvokeAction
{
  NoArgOp   (NoArgFn),
  UnaryOp   (UnaryFn),
  BinaryOp  (BinaryFn),
  TrinityOp (TrinityFn), 
}

#[derive(Clone)]
pub struct FuncSignature 
{
  // Function Name
  name     : String,
  // Function argument data types
  arg_types: Vec<Ty>,
  // Function kind
  fn_kind  : FuncKind,
  // Return type
  ret_type : Ty
}

impl FuncSignature
{
  pub fn new(name: String, arg_types: Vec<Ty>, ret_type: Ty, fn_kind: FuncKind) -> FuncSignature
  {
    FuncSignature {
      name     : name,
      arg_types: arg_types,
      fn_kind  : fn_kind,
      ret_type : ret_type
    }
  }
}

impl Eq for FuncSignature {}

// TODO - compare other attributes
impl PartialEq for FuncSignature {
  fn eq(&self, other: &FuncSignature) -> bool {
    &self.name == &other.name
  }
}

// TODO - compare other attributes
impl PartialOrd for FuncSignature 
{
   fn partial_cmp(&self, other: &FuncSignature) -> Option<Ordering> {
     self.name.partial_cmp(&other.name)
   }
   
   fn lt(&self, other: &FuncSignature) -> bool { self.name < other.name }
   fn le(&self, other: &FuncSignature) -> bool { self.name <= other.name }
   fn gt(&self, other: &FuncSignature) -> bool { self.name > other.name }
   fn ge(&self, other: &FuncSignature) -> bool { self.name <= other.name }
}

// TODO - compare other attributes
impl Ord for FuncSignature {
  fn cmp(&self, other: &FuncSignature) -> Ordering {
    self.name.cmp(&other.name)
  }
}