use std::rc::Rc;

use algebra::{DataSet, Operator};
use common::err::{Result, Void, void_ok};
use common::session::Session;
use common::plugin::{Plugin, PluginManager, TypeRegistry, FuncRegistry};
use plan::{PlanContext, LogicalPlanner};

use super::QueryExecutor;

pub struct LocalQueryExecutor<'a>
{
  plugin_manager: Rc<PluginManager<'a>>,
  logical_planner: LogicalPlanner<'a>
}

impl<'a> LocalQueryExecutor<'a>
{
  pub fn new() -> LocalQueryExecutor<'a>
  {
    let plugin_manager = Rc::new(PluginManager::new());
    LocalQueryExecutor {
      plugin_manager: plugin_manager.clone(),
      logical_planner: LogicalPlanner::new(plugin_manager.clone()),
    }
  }
}

pub struct Client {
  p: Box<QueryExecutor + Copy>
}

impl<'a> QueryExecutor for LocalQueryExecutor<'a>
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
    unimplemented!()
  }
  
  fn close(&self) -> Void {
    void_ok
  }
}