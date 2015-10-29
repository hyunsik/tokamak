use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;
use std::fmt;

use algebra::{Operator, Visitor};
use common::dataset::DataSet;
use common::err::Result;
use common::plugin::{PluginManager, TypeRegistry, FuncRegistry};
use common::session::Session;

use node::*;

pub struct LogicalPlanner;

impl LogicalPlanner
{
  pub fn new() -> LogicalPlanner 
  {
    LogicalPlanner
  }
  
  pub fn build(&self, 
  	type_registry: &TypeRegistry,
  	func_registry: &FuncRegistry, 
  	session: &Session, 
  	algebra: &Operator) -> Result<LogicalPlan>
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