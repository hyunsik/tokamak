use std::cell::RefCell;
use std::rc::Rc;

use algebra::Operator;
use common::dataset::DataSet;
use common::err::{Result, Void, void_ok};
use common::session::Session;
use common::plugin::{FuncRegistry, TypeRegistry, Plugin, PluginManager};
use default_package::DefaultPackage;
use exec::driver::{Driver, DriverContext};
use exec::planner::ExecutionPlanner;
use plan::{LogicalPlanner};
use optimizer::LogicalOptimizer;

use super::QueryRunner;

pub struct LocalQueryRunner<'a>
{
  plugin_manager: PluginManager<'a>,
  planner  : LogicalPlanner,
  optimizer: LogicalOptimizer,
  exec_planner: ExecutionPlanner
}

impl<'a> LocalQueryRunner<'a>
{
  pub fn new() -> LocalQueryRunner<'a>
  {    
    let mut runner = LocalQueryRunner {      
      plugin_manager: PluginManager::new(),
      planner       : LogicalPlanner::new(),
      optimizer     : LogicalOptimizer::new(),
      exec_planner  : ExecutionPlanner::new()
    };
    
    let default_package = Box::new(DefaultPackage);
    default_package.load(&mut runner.plugin_manager);
    
    runner
  }
  
  #[inline]
  fn ty_registry(&self) -> &TypeRegistry {
  	self.plugin_manager.ty_registry()
  }
  
  #[inline]
  fn fn_registry(&self) -> &FuncRegistry {
  	self.plugin_manager.fn_registry()
  }
}

impl<'a> QueryRunner for LocalQueryRunner<'a>
{
	#[inline]
  fn default_session(&self) -> Session 
  {
    Session
  }
  
  fn add_plugin(&mut self, plugin: Box<Plugin>) -> Void {
    // the only place to access the mutable reference of PluginManager
    plugin.load(&mut self.plugin_manager)
    //self.plugin_manager.load(plugin)
  }
  
  #[inline]
  fn plugin_manager(&self) -> &PluginManager
  {
    &self.plugin_manager
  }
  
  fn execute(&self, session: &Session, plan: &Operator) -> Result<DataSet> {
    let logical_plan = try!(self.planner.build(
    		self.ty_registry(), self.fn_registry(), session, plan));
    let optimized    = try!(self.optimizer.optimize(
    		self.ty_registry(), self.fn_registry(), session, &logical_plan));
    let exec_plan    = try!(self.exec_planner.build(
    		self.ty_registry(), self.fn_registry(), session, &optimized));
    
    let ctx = DriverContext::new(self.ty_registry(), self.fn_registry());
    
    let drivers = exec_plan.driver_factories()
    	.iter()
    	.map(|x| x.create_driver(&ctx))
    	.collect::<Vec<Driver>>();
		debug_assert!(drivers.len() > 0, "At least one Driver must be initialized");
    	
    unimplemented!()
  }
  
  fn close(&self) -> Void {
    void_ok
  }
}