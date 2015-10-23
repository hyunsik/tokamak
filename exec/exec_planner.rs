use common::err::{Error, Result};
use driver::DriverFactory;
use plan::*;
use plan::node::RelDecl;
use plan::visitor::*;

use super::ExecutorFactory;

pub struct ExecutionPlan {
  drivers: Vec<Box<DriverFactory>>
}

impl ExecutionPlan {
  pub fn new() -> ExecutionPlan 
  { 
    ExecutionPlan {drivers: Vec::new()} 
  }
}

pub struct ExecutionPlanner;

pub struct ExecPlanContext 
{
  stack: Box<ExecutorFactory>,
  err: Option<Error>
}

impl ExecutionPlanner 
{
  pub fn new() -> ExecutionPlanner 
  {
    ExecutionPlanner
  }
  
  pub fn build(&self, ctx: ExecPlanContext) -> Result<ExecutionPlan>
  {
    match ctx.err {
      Some(e) => Err(e),
      None    => {
        
        let plan = ExecutionPlan {
          drivers: Vec::new()
        };
        
        Ok(plan)
      }
    }
  }
}  

impl<'v> Visitor<'v, ExecPlanContext> for ExecutionPlanner {
  fn visit_relation(&self, ctx: &mut ExecPlanContext, decl: &RelDecl) {}
}