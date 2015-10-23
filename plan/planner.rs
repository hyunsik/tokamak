use std::rc::Rc;
use std::collections::HashMap;
use std::fmt;

use algebra::Operator;
use common::err::Result;
use common::plugin::{PluginManager, TypeRegistry, FuncRegistry};

use node::*;

pub struct LogicalPlanner {
  type_registry: Rc<TypeRegistry>,
  func_registry: Rc<FuncRegistry>
}

impl LogicalPlanner
{
  pub fn new(
    type_registry: Rc<TypeRegistry>,
    func_registry: Rc<FuncRegistry>) -> LogicalPlanner
   
  {
    LogicalPlanner {
      type_registry: type_registry,
      func_registry: func_registry
    }
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