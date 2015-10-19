//! ## Example
//!
//! let ctx = TokamakContext::new();
//! ctx.from(RandomGenerator).select(...);

use uuid::Uuid;

use common::err::{Error, Result};
use common::types::Type;
use common::plan::{CustomDataSource, DataSet, Expr, Plan};
use engine::{create_plan, ExecutionPlan, execute_tasks, parallelize, Task};

use super::TokamakContext;

pub struct DataFrame<'a> {
  pub ctx : &'a TokamakContext,
  pub plan: Plan
}

impl<'a> DataFrame<'a> {
  pub fn kind(&self) -> &'static str {
    match self.plan {
      Plan::From  (_)   => "from",
      Plan::Select(_,_) => "select",
      Plan::Head(_,_)   => "head",
      Plan::Tail(_,_)   => "tail",
      _                 => "Unknown"
    } 
  }
  
  pub fn select(self, exprs: Vec<Expr>) -> DataFrame<'a> {
    DataFrame {ctx: self.ctx, plan: Plan::Select(Box::new(self.plan), exprs)}
  }
  
  pub fn count(&self) -> Result<usize> {
//    let count_plan = Plan::Aggregate(self.decl, vec![], vec!["count(*)"]);
//    try!(execute(count_plan)).get_int8(0)
    Err(Error::NotImplemented)
  }
  
  pub fn head(self) -> Result<Box<DataSet>> {
    self.head_with(1)
  }
  
  pub fn head_with(self, num: usize) -> Result<Box<DataSet>> {
    let head_plan = Plan::Head(Box::new(self.plan), num);
    execute(self.ctx, head_plan)
  }
  
  pub fn tail(self) -> Result<Box<DataSet>> {
    self.tail_with(1)
  }
  
  pub fn tail_with(self, num: usize) -> Result<Box<DataSet>> {
    let tail_plan = Plan::Tail(Box::new(self.plan), num);
    execute(self.ctx, tail_plan)
  }
}

fn execute(ctx: &TokamakContext, plan: Plan) -> Result<Box<DataSet>> {
  let exec_plan : ExecutionPlan = try!(create_plan(ctx.package_manager(), &plan));
  //let drivers = exec_plan.
  //let tasks: Vec<Task>     = try!(parallelize(&exec_plan));  
  //execute_tasks(tasks)
  
  Err(Error::NotImplemented)
}


pub fn RandomGenerator(types: Vec<&str>, rownum: usize) -> Box<DataSet>
{
  Box::new(CustomDataSource::new(
    &Uuid::new_v4().to_hyphenated_string(),  
    "random",
    types,
    Vec::new()
  ))
}