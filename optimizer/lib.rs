//!
//! Planner and Optimizer

extern crate common;
extern crate plan;

use std::rc::Rc;

use common::err::Result;
use common::session::Session;
use common::plugin::{FuncRegistry, TypeRegistry};
use plan::LogicalPlan;

pub struct LogicalOptimizer 
{
  type_registry: Rc<TypeRegistry>,
  func_registry: Rc<FuncRegistry>
}

impl LogicalOptimizer 
{
  pub fn new(
    type_registry: Rc<TypeRegistry>,
    func_registry: Rc<FuncRegistry>) -> LogicalOptimizer 
  {
    LogicalOptimizer {
      type_registry: type_registry,
      func_registry: func_registry
    }
  }
  
  pub fn optimize(&self, session: &Session, plan: &LogicalPlan) -> Result<LogicalPlan> {
    let x = (*plan).clone();
    Ok(x)
  }
}