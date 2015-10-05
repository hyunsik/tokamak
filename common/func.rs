//! Function Registry

use std::collections::BTreeMap;

use super::types::Type;
use super::rows::{MiniPage,PageBuilder};

pub enum FuncType {
  Operator,
  Scalar,
  Aggregation,
  Window
}

pub type ScalarVecFunc = Fn(Vec<&MiniPage>, &mut PageBuilder);

pub enum InvokeAction
{
  ScalarVec(Box<ScalarVecFunc>) 
}

pub struct FuncSignature 
{
  name: String,
  params: Vec<Box<Type>>,
  func_type: FuncType
}

pub struct FuncRegistry
{
  // key and value will be kept immutable as a just reference
  types: BTreeMap<FuncSignature, InvokeAction>
}