use std::rc::Rc;
use std::collections::HashMap;
use std::fmt;

use algebra::Operator;
use common::err::Result;
use common::plugin::{PluginManager, TypeRegistry, FuncRegistry};

use node::*;

pub trait PlanContext<'a> {
  fn type_registry(&'a self) -> &'a TypeRegistry;
  
  fn func_registry(&'a self) -> &'a FuncRegistry;
}


pub struct LogicalPlanner<'a> {
  plugin_manager: Rc<PluginManager<'a>>
}

impl<'a> LogicalPlanner<'a>
{
  pub fn new(plugin_manager: Rc<PluginManager<'a>>) -> LogicalPlanner<'a> 
  {
    LogicalPlanner {
      plugin_manager: plugin_manager
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