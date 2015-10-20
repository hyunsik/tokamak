use common::err::Result;
use common::plan::Plan;

pub struct LogicalPlan;

pub trait LogicalPlanner {
  fn create(plan: &Plan) -> Result<LogicalPlan>;
}

pub trait LogicalOptimizer {
  fn optimize(plan: &LogicalPlan) -> Result<LogicalPlan>;
}

pub struct Dag;

pub trait DagBuilder {
  fn create(plan: &LogicalPlan) -> Result<Dag>;
}

pub trait DagOptimizer {
  fn optimize(dag: &Dag) -> Result<Dag>;
}