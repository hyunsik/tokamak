use std::marker::PhantomData;

use common::err::{Error, TResult};
use common::plan::{Plan, Visitor};
use common::plugin::{PackageManager};

pub fn create_plan(pkg_mgr: &PackageManager, plan: &Plan) -> TResult<ExecutionPlan> {
  Err(Error::NotImplemented)
}

pub struct ExecutionPlanner;

impl<'a> Visitor<'a> for ExecutionPlanner {
}

pub struct ExecutionPlan;
