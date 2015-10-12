//! Function Registry

use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::collections::btree_map::Entry;
use std::collections::btree_map::Entry::{Occupied, Vacant};
use std::cell::RefCell;

use err::{Void, void_ok, Error};
use types::Type;
use rows::{MiniPage,MiniPageWriter};

#[derive(Eq, Copy, Clone, PartialEq, PartialOrd, Ord)]
pub enum FuncType {
  Operator,
  Scalar,
  Aggregation,
  Window
}

pub type UnaryFn   = Fn(&MiniPage, RefCell<MiniPageWriter>);
pub type BinaryFn  = Fn(&MiniPage, &MiniPage, RefCell<MiniPageWriter>);
pub type TrinityFn = Fn(&MiniPage, &MiniPage, &MiniPage, RefCell<MiniPageWriter>);

pub type ScalarVecFunc = Fn(Vec<&MiniPage>, RefCell<MiniPageWriter>);


pub enum InvokeAction
{
  UnaryOp  (Box<UnaryFn>),
  BinaryOp (Box<BinaryFn>),
  TrinityOp(Box<TrinityFn>),
  
  ScalarVec(Box<ScalarVecFunc>) 
}

#[derive(Clone)]
pub struct FuncSignature 
{
  name: String,
  params: Vec<Box<Type>>,
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

pub struct FuncRegistry
{
  // key and value will be kept immutable as a just reference
  funcs: BTreeMap<FuncSignature, InvokeAction>
}

impl FuncRegistry 
{
  pub fn new() -> FuncRegistry 
  {
    FuncRegistry {
      funcs: BTreeMap::new()
    }    
  }
  
  pub fn add_all(&mut self, funcs: Vec<(FuncSignature, InvokeAction)>) -> Void {
    for x in funcs.into_iter() {
      self.funcs.insert(x.0, x.1);
    }
    
    for (sig, invoke) in funcs.into_iter() {
      match self.funcs.entry(sig) {
        Vacant(e)   => { 
          e.insert(invoke); 
        },
        Occupied(_) => { return Err(Error::DuplicatedFuncSign) }
      }      
    }
    
    void_ok()
  }
}