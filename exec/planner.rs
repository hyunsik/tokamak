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

pub struct ExecutionPlan<'a> {
  driver_factories: Vec<DriverFactory<'a>>
}

impl<'a> ExecutionPlan<'a> {
  pub fn new(driver_factories: Vec<DriverFactory<'a>>) -> ExecutionPlan<'a>
  { 
    ExecutionPlan {
    	driver_factories: driver_factories
  	} 
  }
  
  pub fn driver_factories(&self) -> &Vec<DriverFactory<'a>> {
  	&self.driver_factories
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
    let mut ctx = ExecPlanContext {
      stack: Vec::new(),
      err  : None
    };
    
    walk_node(self, &mut ctx, plan.root());
    
    let driver_factory = DriverFactory::new(
    	true,
    	true,
    	ctx.stack
    );
    
    match ctx.err {
      Some(e) => Err(e),
      None    => {
        
        let plan = ExecutionPlan::new(vec![driver_factory]);
        
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