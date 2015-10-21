use std::fmt;

use algebra::{Operator, DataSet};
use common::err::{Result, Void, void_ok};
use plan::{Bindable, SchemaObject, Plan, PlanContext};
use common::plugin::{Plugin, PluginManager};
use common::types::Type;
use common::session::Session;

pub struct MaterializedResult 
{
  name: String,
  schema: Vec<Box<Type>>
}

impl DataSet for MaterializedResult
{
  fn id(&self) -> &str 
  {
    &self.name
  }
  
  fn kind(&self) -> &str
  {
    "table"
  }
  
  fn schema(&self) -> Vec<&str> {
    self.schema.iter()
      .map(|t| t.id().base())
      .collect::<Vec<&str>>()
  }
  
  fn uri(&self) -> Option<&str> {
    None
  }
}

impl Bindable for MaterializedResult
{
  fn bind(&mut self, ctx: &PlanContext) -> Void
  {
    void_ok
  }
}

impl SchemaObject for MaterializedResult
{
  fn schema(&self) -> &Vec<Box<Type>>
  {
    &self.schema
  }
}

impl fmt::Display for MaterializedResult {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "name={}", self.name)
  }
}

pub trait QueryExecutor 
{
  
  fn default_session(&self) -> Session;
  
  fn add_plugin(&mut self, package: Box<Plugin>) -> Void;
  
  fn plugin_manager(&self) -> &PluginManager; 
  
  fn execute(&self, session: &Session, plan: &Operator) -> Result<Box<DataSet>>;
  
  fn close(&self) -> Void;
}

pub struct LocalQueryExecutor
{
  plugin_manager: PluginManager
}

impl LocalQueryExecutor
{
  pub fn new() -> LocalQueryExecutor
  {
    LocalQueryExecutor {
      plugin_manager: PluginManager::new()
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