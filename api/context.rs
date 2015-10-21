use algebra::{DataSet, Operator};
use common::err::Result;
use common::session::Session;
use common::types::Type;
use common::plugin::PluginManager;
use engine::{LocalQueryExecutor, QueryExecutor};

use sql::SQLPackage;

use df::{DataFrame};

pub struct TokamakContext 
{
  pub session : Session,
  executor: Box<QueryExecutor>
}

impl TokamakContext 
{
  pub fn new() -> Result<TokamakContext> 
  {
    let mut executor = Box::new(LocalQueryExecutor::new());
    try!(executor.add_plugin(Box::new(SQLPackage)));
    
    Ok(TokamakContext {
      session: executor.default_session(),
      executor: executor
    })
  }
  
  pub fn runner(&self) -> &QueryExecutor {
    &*self.executor
  } 
  
  pub fn plugin_manager(&self) -> &PluginManager { &self.executor.plugin_manager() }
  
  #[inline]
  pub fn get_type(&self, type_sign: &str) -> Result<Box<Type>>
  {
    self.plugin_manager().type_registry().get(type_sign)
  }
  
  #[inline]
  pub fn all_types(&self) -> Vec<&str> 
  {
    self.plugin_manager().type_registry().all()
  }
  
  pub fn from(&self, ds: Box<DataSet>) -> DataFrame {
    DataFrame {ctx: self, plan: Operator::Scan(ds)}
  }
}