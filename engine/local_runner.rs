use std::rc::Rc;

use algebra::{DataSet, Operator};
use common::err::{Result, Void, void_ok};
use common::session::Session;
use common::plugin::{Plugin, PluginManager};
use exec::planner::ExecutionPlanner;
use plan::{LogicalPlanner};
use optimizer::LogicalOptimizer;

use super::QueryRunner;

pub struct LocalQueryRunner<'a>
{
  plugin_manager: Rc<PluginManager<'a>>,
  planner  : LogicalPlanner,
  optimizer: LogicalOptimizer,
  exec_planner: ExecutionPlanner
}

impl<'a> LocalQueryRunner<'a>
{
  pub fn new() -> LocalQueryRunner<'a>
  {
    let plugin_manager = Rc::new(PluginManager::new());
    
    LocalQueryRunner {
      
      plugin_manager: plugin_manager.clone(),
      
      planner       : LogicalPlanner::new(
                         plugin_manager.type_registry(), 
                         plugin_manager.func_registry()
                      ),
      optimizer     : LogicalOptimizer::new(
                         plugin_manager.type_registry(), 
                         plugin_manager.func_registry()
                      ),
      exec_planner  : ExecutionPlanner::new(
                         plugin_manager.type_registry(), 
                         plugin_manager.func_registry()
                      )
    }
  }
}

impl<'a> QueryRunner for LocalQueryRunner<'a>
{
  fn default_session(&self) -> Session 
  {
    Session
  }
  
  fn add_plugin(&mut self, plugin: Box<Plugin>) -> Void {
    // the only place to access the mutable reference of PluginManager
    Rc::get_mut(&mut self.plugin_manager).unwrap().load(plugin)
  }
  
  fn plugin_manager(&self) -> &PluginManager
  {
    &self.plugin_manager
  }
  
  fn execute(&self, session: &Session, plan: &Operator) -> Result<Box<DataSet>> {
    let logical_plan = try!(self.planner.build(session, plan));
    let optimized    = try!(self.optimizer.optimize(session, &logical_plan));
    let exec_plan    = try!(self.exec_planner.build(session, &optimized));
    
    unimplemented!()
  }
  
  fn close(&self) -> Void {
    void_ok
  }
}