use std::fmt;
use std::rc::Rc;

use algebra::{Operator, DataSet};
use common::err::{Result, Void, void_ok};
use common::plugin::{Plugin, PluginManager};
use common::types::Ty;
use common::session::Session;

pub struct MaterializedResult 
{
  name: String,
  schema: Vec<Box<Ty>>,
  raw_schema: Vec<String>
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
  
  fn schema(&self) -> &Vec<String> {
    &self.raw_schema
  }
  
  fn uri(&self) -> Option<&str> {
    None
  }
}

impl fmt::Display for MaterializedResult {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "name={}", self.name)
  }
}

pub trait QueryRunner 
{
  
  fn default_session(&self) -> Session;
  
  fn add_plugin(&mut self, package: Box<Plugin>) -> Void;
  
  fn plugin_manager(&self) -> &PluginManager; 
  
  fn execute(&self, session: &Session, plan: &Operator) -> Result<Box<DataSet>>;
  
  fn close(&self) -> Void;
}