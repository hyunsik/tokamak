use algebra::Operator;
use common::dataset::DataSet;
use common::err::{Result, Void};
use common::plugin::{Plugin, PluginManager};
use common::session::Session;

pub trait QueryRunner
{

  fn default_session(&self) -> Session;

  fn add_plugin(&mut self, package: Box<Plugin>) -> Void;

  fn plugin_manager(&self) -> &PluginManager;

  fn execute(&self, session: &Session, plan: &Operator) -> Result<DataSet>;

  fn close(&self) -> Void;
}
