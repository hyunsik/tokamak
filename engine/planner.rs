use std::collections::HashSet;
use std::marker::PhantomData;

use common::err::{Error, Result};
use common::plan::*;
use common::plugin::{PluginManager};

use exec::ExecutorFactory;
use exec::scan::TableScanExecFactory;

pub fn create_plan(pkg_mgr: &PluginManager, plan: &Plan) -> Result<ExecutionPlan> {
  let mut planner = ExecutionPlanner::new();
  walk_plan(&mut planner, plan);
  planner.plan()
}

#[derive(PartialEq, Eq, Hash)]
pub struct Split;

pub type PlanNodeId = String;

pub struct Driver;

pub struct TaskSource {
  plan_node_id: PlanNodeId,
  splits      : HashSet<Split>
}

impl Driver
{
  pub fn update_source(&self, source: TaskSource) {}
}

pub struct DriverFactory 
{
  pub input_driver: bool,
  pub output_driver: bool, 
  source_ids: Vec<String>,
  factory: Vec<Box<DriverFactory>>
}

pub struct DriverContext;

impl DriverFactory {
  pub fn create_driver(&self, ctx: &DriverContext) -> Driver
  {
    Driver
  }
}

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
  factories: Vec<Box<ExecutorFactory>>,
  err: Option<Error>  
}

impl ExecutionPlanner 
{
  pub fn new() -> ExecutionPlanner 
  {
    ExecutionPlanner { 
      factories: Vec::new(),
      err : None 
    }
  }
  
  pub fn plan(self) -> Result<ExecutionPlan>
  {
    match self.err {
      Some(e) => Err(e),
      None    => {
        
        let plan = ExecutionPlan {
          drivers: Vec::new()
        };
        
        Ok(plan)
      }
    }
  }
  
  pub fn push(&mut self, f: Box<ExecutorFactory>) {
    self.factories.push(f)
  }
  
  pub fn pop(&mut self) -> Option<Box<ExecutorFactory>> {
    self.factories.pop()
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


