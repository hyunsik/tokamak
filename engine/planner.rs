use common::err::{Error, TResult};
use common::plan::Plan;
use common::plugin::{PackageManager};

pub fn create_plan(pkg_mgr: &PackageManager, plan: &Plan) -> TResult<ExecutionPlan> {
  Err(Error::NotImplemented)
}

pub struct ExecutionPlan;