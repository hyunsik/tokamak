use common::types::Ty;
use driver::DriverContext;
use super::{Executor, ExecutorFactory};

pub struct FilterExecFactory {
  types: Vec<Ty>,
}

impl FilterExecFactory {
  pub fn new(f: Box<ExecutorFactory>) -> FilterExecFactory {
    FilterExecFactory { types: Vec::new() }
  }
}

impl ExecutorFactory for FilterExecFactory {
  fn create(&self, ctx: &DriverContext) -> Option<Box<Executor>> {
    None
  }

  fn types(&self) -> &Vec<Ty> {
    &self.types
  }
}

pub struct FilterExec;
