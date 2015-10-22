//!
//! Planner and Optimizer

extern crate common;
extern crate plan;

use common::err::Result;
use plan::LogicalPlan;

pub struct LogicalOptimizer;

impl LogicalOptimizer 
{
  pub fn optimize(plan: &LogicalPlan) -> Result<LogicalPlan> {
    let x = (*plan).clone();
    Ok(x)
  }
}