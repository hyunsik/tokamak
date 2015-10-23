use algebra::{DataSet, Operator};
use common::err::{Result, Void, void_ok};
use common::session::Session;
use common::plugin::{Plugin, PluginManager};
use plan::LogicalPlanner;

use super::QueryExecutor;

pub struct LocalQueryExecutor
{
  plugin_manager: PluginManager,
  logical_planner: LogicalPlanner
}

impl LocalQueryExecutor
{
  pub fn new() -> LocalQueryExecutor
  {
    LocalQueryExecutor {
      plugin_manager: PluginManager::new(),
      logical_planner: LogicalPlanner::new()
    }
  }
}

impl QueryExecutor for LocalQueryExecutor
{
  fn default_session(&self) -> Session 
  {
    Session
  }
  
  fn add_plugin(&mut self, plugin: Box<Plugin>) -> Void {
    self.plugin_manager.load(plugin)
  }
  
  fn plugin_manager(&self) -> &PluginManager
  {
    &self.plugin_manager
  }
  
  fn execute(&self, session: &Session, plan: &Operator) -> Result<Box<DataSet>> {
    unimplemented!()
  }
  
  fn close(&self) -> Void {
    void_ok
  }
}