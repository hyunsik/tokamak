//!
//! Plan
//!

extern crate common;
extern crate algebra;

use std::collections::HashMap;
use std::fmt;

use algebra::Operator;
use common::types::Type;
use common::err::{Error, Result, Void, void_ok};
use common::plugin::{TypeRegistry, FuncRegistry};

pub mod node;
use node::*;

pub mod expr;

pub mod visitor;
use visitor::*;

pub struct LogicalPlanner;

impl LogicalPlanner
{
  pub fn new() -> LogicalPlanner 
  {
    LogicalPlanner
  }
  
  pub fn create(&self, algebra: &Operator) -> Result<LogicalPlan>
  {
    unimplemented!();
  } 
}

#[derive(Clone)]
pub struct QueryBlock {
  root: PlanNode
}

#[derive(Clone)]
pub struct LogicalPlan {
  query_blocks: HashMap<i32, QueryBlock> 
}

pub trait PlanContext {
  fn type_registry(&self) -> &TypeRegistry;
  fn func_registry(&self) -> &FuncRegistry;
}

/*
fn typestr_to_schema(ctx: &PlanContext, types: &Vec<String>) -> Vec<Box<Type>>
{
  types.iter()
    .map( |s| ctx.type_registry().get(s).unwrap().clone_box() )
    .collect::<Vec<Box<Type>>>()
}
*/