use common::err::{Error, TResult};
use common::plugin::{PackageManager};

pub struct ExecutionPlanner;

impl ExecutionPlanner 
{
  pub fn new() -> ExecutionPlanner {  
    ExecutionPlanner
  }
  
  pub fn plan(&self, pkg_mgr: &PackageManager) -> TResult<ExecutionPlan> {
    Err(Error::NotImplemented)
  }
}

pub struct ExecutionPlan;