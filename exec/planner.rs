use std::cell::RefCell;
use std::rc::Rc;

use common::dataset::{DataSet, DataSetDecl};
use common::err::{Error, Result};
use common::plugin::{FuncRegistry, TypeRegistry};
use common::session::Session;

use plan::*;
use plan::visitor::*;

use driver::DriverFactory;
use scan::TableScanExecFactory;
use storage::get_factory;
use super::ExecutorFactory;

pub struct ExecutionPlan {
  drivers: Vec<Box<DriverFactory>>
}

impl ExecutionPlan {
  pub fn new() -> ExecutionPlan 
  { 
    ExecutionPlan {drivers: Vec::new()} 
  }
}

pub struct ExecutionPlanner;

pub struct ExecPlanContext 
{
  stack: Vec<Box<ExecutorFactory>>,
  err: Option<Error>
}

impl ExecutionPlanner 
{
  pub fn new() -> ExecutionPlanner 
  {
    ExecutionPlanner
  }
  
  pub fn build(&self, 
  	type_registry: &TypeRegistry,
  	func_registry: &FuncRegistry, 
  	session: &Session, plan: &LogicalPlan) -> Result<ExecutionPlan>
  {
    let ctx = ExecPlanContext {
      stack: Vec::new(),
      err  : None
    };
    
    match ctx.err {
      Some(e) => Err(e),
      None    => {
        
        let plan = ExecutionPlan {
          drivers: Vec::new()
        };
        
        Ok(plan)
      }
    }
  }
}  

impl<'v> Visitor<'v, ExecPlanContext> for ExecutionPlanner {
  fn visit_relation(&self, ctx: &mut ExecPlanContext, ds: &'v DataSet) {
  	
  	match ds.decl {
  		DataSetDecl::RandomTable(ref types, rownum) => {
  			let factory = Box::new(TableScanExecFactory {
  				types: types.clone(),
  				source_factory: get_factory("random")
  			});
  			ctx.stack.push(factory);
  		}
  	}
  	
  	
 	}
}