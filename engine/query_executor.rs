use std::fmt;

use common::err::{Result, Void, void_ok};
use common::plan::{Bindable, DataSet, SchemaObject, Plan, PlanContext};
use common::plugin::{Package, PackageManager};
use common::types::Type;
use common::session::Session;

pub struct MaterializedResult 
{
  name: String,
  schema: Vec<Box<Type>>
}

impl DataSet for MaterializedResult
{
  fn name(&self) -> &str 
  {
    &self.name
  }
  
  fn kind(&self) -> &str
  {
    "table"
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
  
  fn add_plugin(&mut self, package: Box<Package>) -> Void;
  
  fn plugin_manager(&self) -> &PackageManager; 
  
  fn execute(&self, session: &Session, plan: &Plan) -> Result<Box<DataSet>>;
  
  fn close(&self) -> Void;
}

pub struct LocalQueryExecutor
{
  plugin_manager: PackageManager
}

impl LocalQueryExecutor
{
  pub fn new() -> LocalQueryExecutor
  {
    LocalQueryExecutor {
      plugin_manager: PackageManager::new()
    }
  }
}

impl QueryExecutor for LocalQueryExecutor
{
  fn default_session(&self) -> Session 
  {
    Session
  }
  
  fn add_plugin(&mut self, plugin: Box<Package>) -> Void {
    self.plugin_manager.load(plugin)
  }
  
  fn plugin_manager(&self) -> &PackageManager
  {
    &self.plugin_manager
  }
  
  fn execute(&self, session: &Session, plan: &Plan) -> Result<Box<DataSet>> {
    unimplemented!()
  }
  
  fn close(&self) -> Void {
    void_ok
  }
}