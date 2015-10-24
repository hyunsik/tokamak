use std::rc::Rc;

use common::err::{Error, Result};
use common::plugin::{FuncRegistry, TypeRegistry};
use common::session::Session;

use plan::*;
use plan::node::RelDecl;
use plan::visitor::*;

use driver::DriverFactory;

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

pub struct ExecutionPlanner {
  type_registry: Rc<TypeRegistry>,
  func_registry: Rc<FuncRegistry>
}

pub struct ExecPlanContext 
{
  stack: Vec<Box<ExecutorFactory>>,
  err: Option<Error>
}

impl ExecutionPlanner 
{
  pub fn new(
    type_registry: Rc<TypeRegistry>,
    func_registry: Rc<FuncRegistry>) -> ExecutionPlanner 
  {
    ExecutionPlanner {
      type_registry: type_registry,
      func_registry: func_registry
    }
  }
  
  pub fn build(&self, session: &Session, plan: &LogicalPlan) -> Result<ExecutionPlan>
  {
    let ctx = ExecPlanContext {
      stack: Vec::new(),
      err  : None
    };
    
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