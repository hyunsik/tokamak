use common::err::TResult;

pub struct Parallelizer;

impl Parallelizer {
  pub fn new() -> Parallelizer
  {
    Parallelizer
  }
  
  pub fn parallelize(plan &ExecutionPlan) -> TResult<Vec<Task>>
  {
    None
  }
}

