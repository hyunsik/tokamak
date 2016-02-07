use common::types::Ty;
use driver::DriverContext;
use super::{Executor, ExecutorFactory};

pub struct HashJoinExecFactory {
  types: Vec<Ty>,
}

impl HashJoinExecFactory {
  pub fn new(lf: Box<ExecutorFactory>, rf: Box<ExecutorFactory>) -> HashJoinExecFactory {
    HashJoinExecFactory { types: Vec::new() }
  }
}

impl ExecutorFactory for HashJoinExecFactory {
  fn create(&self, ctx: &DriverContext) -> Option<Box<Executor>> {
    None
  }

  fn types(&self) -> &Vec<Ty> {
    &self.types
  }
}

pub struct HashJoinExec;
