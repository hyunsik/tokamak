use common::plugin::{TypeRegistry, FuncRegistry};

pub struct ExecutionPlanner<'a>
{
  type_registry: &'a TypeRegistry,
  func_registry: &'a FuncRegistry 
}

impl<'a> ExecutionPlanner<'a> {
  pub fn new(type_regs: &TypeRegistry) {
  }
}

pub struct ExecutionPlan;