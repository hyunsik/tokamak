//! Function Registry

use std::cmp::Ordering;
use std::rc::Rc;
use std::cell::RefCell;

use rows::{MiniPage,MiniPageWriter};

#[derive(Eq, Copy, Clone, PartialEq, PartialOrd, Ord)]
pub enum FuncType {
  Operator,
  Scalar,
  Aggregation,
  Window
}

pub type UnaryFn   = Rc<Fn(&MiniPage, RefCell<MiniPageWriter>)>;
pub type BinaryFn  = Rc<Fn(&MiniPage, &MiniPage, RefCell<MiniPageWriter>)>;
pub type TrinityFn = Rc<Fn(&MiniPage, &MiniPage, &MiniPage, RefCell<MiniPageWriter>)>;

pub type ScalarVecFunc = Rc<Fn(Vec<&MiniPage>, RefCell<MiniPageWriter>)>;

#[derive(Clone)]
pub enum InvokeAction
{
  UnaryOp  (UnaryFn),
  BinaryOp (BinaryFn),
  TrinityOp(TrinityFn),
  
  ScalarVec(ScalarVecFunc) 
}

#[derive(Clone)]
pub struct FuncSignature 
{
  name: String,
  func_type: FuncType
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