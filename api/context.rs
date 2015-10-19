use common::err::Result;
use common::session::Session;
use common::types::Type;
use common::plan::{DataSet, Plan, PlanContext};
use common::plugin::{PackageManager, TypeRegistry, FuncRegistry};
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
    executor.add_plugin(Box::new(SQLPackage));
    
    let session  = executor.default_session();
    
    Ok(TokamakContext {
      session: session,
      executor: executor
    })
  }
  
  pub fn runner(&self) -> &QueryExecutor {
    &*self.executor
  } 
  
  pub fn plugin_manager(&self) -> &PackageManager { &self.executor.plugin_manager() }
  
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
    DataFrame {ctx: self, plan: Plan::From(ds)}
  }
}

impl PlanContext for TokamakContext 
{
  fn type_registry(&self) -> &TypeRegistry 
  {
    self.plugin_manager().type_registry()
  }
  
  fn func_registry(&self) -> &FuncRegistry 
  {
    self.plugin_manager().func_registry()
  }
}