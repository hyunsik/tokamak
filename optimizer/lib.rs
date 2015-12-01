//!
//! Planner and Optimizer

extern crate common;
extern crate plan;

use common::err::Result;
use common::session::Session;
use common::plugin::{FuncRegistry, TypeRegistry};
use plan::LogicalPlan;

pub struct LogicalOptimizer;

impl LogicalOptimizer 
{
  pub fn new() -> LogicalOptimizer 
  {
    LogicalOptimizer
  }
  
  #[allow(unused_variables)]
  pub fn optimize(&self, 
  	type_registry: &TypeRegistry,
  	func_registry: &FuncRegistry,
  	session: &Session, plan: &LogicalPlan) -> Result<LogicalPlan> {
    let x = (*plan).clone();
    Ok(x)
  }
}