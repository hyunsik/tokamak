use std::rc::Rc;
use std::collections::HashMap;
use std::fmt;

use algebra::{DataSet, Operator, Visitor};
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
    let builder = PlanBuilder::new();
    unimplemented!();
  } 
}

pub struct PlanBuilder {
  stack: Vec<PlanNode>
}

impl PlanBuilder 
{
  pub fn new() -> PlanBuilder 
  {
    PlanBuilder {stack: Vec::new()}
  } 
}

impl<'v> Visitor<'v, PlanBuilder> for LogicalPlanner {
  fn visit_dataset(&self, ctx: &mut PlanBuilder, dataset: &'v DataSet) {
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