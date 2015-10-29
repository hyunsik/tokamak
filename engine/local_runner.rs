use std::cell::RefCell;
use std::rc::Rc;

use algebra::{DataSet, Operator};
use common::err::{Result, Void, void_ok};
use common::session::Session;
use common::plugin::{FuncRegistry, TypeRegistry, Plugin, PluginManager};
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
    LocalQueryRunner {      
      plugin_manager: PluginManager::new(),
      planner       : LogicalPlanner::new(),
      optimizer     : LogicalOptimizer::new(),
      exec_planner  : ExecutionPlanner::new()
    }
  }
  
  #[inline]
  fn type_registry(&self) -> &TypeRegistry {
  	self.type_registry()
  }
  
  #[inline]
  fn func_registry(&self) -> &FuncRegistry {
  	self.func_registry()
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
    self.plugin_manager.load(plugin)
  }
  
  #[inline]
  fn plugin_manager(&self) -> &PluginManager
  {
    &self.plugin_manager
  }
  
  fn execute(&self, session: &Session, plan: &Operator) -> Result<Box<DataSet>> {
    let logical_plan = try!(self.planner.build(
    		self.type_registry(), self.func_registry(), session, plan));
    let optimized    = try!(self.optimizer.optimize(
    		self.type_registry(), self.func_registry(), session, &logical_plan));
    let exec_plan    = try!(self.exec_planner.build(
    		self.type_registry(), self.func_registry(), session, &optimized));
    
    unimplemented!()
  }
  
  fn close(&self) -> Void {
    void_ok
  }
}