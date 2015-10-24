use std::rc::Rc;
use std::collections::HashMap;
use std::fmt;

use algebra::{Operator, Visitor};
use common::err::Result;
use common::plugin::{PluginManager, TypeRegistry, FuncRegistry};
use common::session::Session;

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
  
  pub fn build(&self, session: &Session, algebra: &Operator) -> Result<LogicalPlan>
  {
    unimplemented!();
  } 
}

pub struct PlanContext {
  
}

impl<'v> Visitor<'v, PlanContext> {
  fn visit_dataset(&self, &mut PlanContext, dataset: &'v DataSet) {
  }
}

#[derive(Clone)]
pub struct QueryBlock {
  id  : u32,
  root: PlanNode
}

#[derive(Clone)]
pub struct LogicalPlan {
  root_id     : u32,
  query_blocks: HashMap<u32, QueryBlock> 
}