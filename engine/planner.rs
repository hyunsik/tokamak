use std::collections::HashSet;
use std::marker::PhantomData;

use common::err::{Error, Result};
use common::plan::*;
use common::plugin::{PluginManager};

use exec::ExecutorFactory;
use exec::scan::TableScanExecFactory;

pub fn create_plan(pkg_mgr: &PluginManager, plan: &Plan) -> Result<ExecutionPlan> {
  let mut planner = ExecutionPlanner::new();
  let mut ctx = ExecPlanContext {err: None};
  { walk_plan(&planner, &mut ctx, plan); }
  planner.plan(ctx)
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

pub struct ExecutionPlanner;

impl ExecutionPlanner 
{
  pub fn new() -> ExecutionPlanner 
  {
    ExecutionPlanner
  }
  
  pub fn plan(&self, ctx: ExecPlanContext) -> Result<ExecutionPlan>
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
  
//  pub fn push(&mut self, f: Box<ExecutorFactory>) {
//    self.factories.push(f)
//  }
//  
//  pub fn pop(&mut self) -> Option<Box<ExecutorFactory>> {
//    self.factories.pop()
//  }
}

pub struct ExecPlanContext {
  err: Option<Error>
}

impl<'a, ExecPlanContext> Visitor<'a, ExecPlanContext> for ExecutionPlanner 
{
  fn visit_from(&self, ctx: &mut ExecPlanContext, ds: &DataSet) 
  {
    //self.push(Box::new(TableScanExecFactory::new(Vec::new())))
  }

  fn visit_head(&self, ctx: &mut ExecPlanContext, child: &Plan, rownum: usize) 
  {
    walk_plan(self, ctx, child);
  }
  
  fn visit_tail(&self, ctx: &mut ExecPlanContext, child: &Plan, rownum: usize) 
  {
    walk_plan(self, ctx, child);
  }
}


