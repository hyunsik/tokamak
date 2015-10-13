use common::err::{Error, TResult};
use common::plugin::{TypeRegistry, FuncRegistry};

pub struct ExecutionPlanner<'a>
{
  type_regs: &'a TypeRegistry,
  func_regs: &'a FuncRegistry 
}

impl<'a> ExecutionPlanner<'a> 
{
  pub fn new(
    type_regs: &'a TypeRegistry,
    func_regs: &'a FuncRegistry) -> ExecutionPlanner<'a> {
      
    ExecutionPlanner {
      type_regs: type_regs,
      func_regs: func_regs
    }
  }
  
  pub fn plan(&self) -> TResult<ExecutionPlan> {
    Err(Error::NotImplemented)
  }
}

pub struct ExecutionPlan;