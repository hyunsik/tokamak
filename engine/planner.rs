use std::marker::PhantomData;

use common::err::{Error, TResult};
use common::plan::*;
use common::plugin::{PackageManager};

use exec::ExecutorFactory;
use exec::scan::TableScanExecFactory;

pub fn create_plan(pkg_mgr: &PackageManager, plan: &Plan) -> TResult<ExecutionPlan> {
  let mut planner = ExecutionPlanner::new();
  walk_plan(&mut planner, plan);
  planner.plan()
}

pub struct ExecutionPlan {
  stack: Vec<Box<ExecutorFactory>>
}

impl ExecutionPlan {
  pub fn new() -> ExecutionPlan 
  { 
    ExecutionPlan {stack: Vec::new()} 
  }
}

pub struct ExecutionPlanner {
  plan: ExecutionPlan,
  err: Option<Error>  
}

impl ExecutionPlanner 
{
  pub fn new() -> ExecutionPlanner 
  {
    ExecutionPlanner { 
      plan: ExecutionPlan::new(),
      err : None 
    }
  }
  
  pub fn plan(self) -> TResult<ExecutionPlan>
  {
    match self.err {
      Some(e) => Err(e),
      None    => Ok(self.plan)
    }
  }
  
  pub fn push(&mut self, f: Box<ExecutorFactory>) {
    self.plan.stack.push(f)
  }
}

impl<'a> Visitor<'a> for ExecutionPlanner 
{
  fn visit_from(&mut self, ds: &DataSet) 
  {
    self.push(Box::new(TableScanExecFactory::new(Vec::new())))
  }

  fn visit_head(&mut self, child: &Plan, rownum: usize) 
  {
    walk_plan(self, child);
  }
  
  fn visit_tail(&mut self, child: &Plan, rownum: usize) 
  {
    walk_plan(self, child);
  }
}


