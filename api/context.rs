use std::rc::Rc;

use algebra::{DataSet, Operator};
use common::err::Result;
use common::session::Session;
use common::types::Ty;
use common::plugin::PluginManager;
use engine::{LocalQueryRunner, QueryRunner};

use sql::SQLPackage;

use df::{DataFrame};

pub struct TokamakContext 
{
  pub session : Session,
  executor: Box<QueryRunner>
}

impl TokamakContext 
{
  pub fn new() -> Result<TokamakContext> 
  {
    println!("Initialize LocalQueryRunner");
    let mut executor = Box::new(LocalQueryRunner::new());
    
    println!("Trying to load SQLPackage");
    try!(executor.add_plugin(Box::new(SQLPackage)));
    println!("SQLPackage plugin has been successfully loaded.");
    
    Ok(TokamakContext {
      session: executor.default_session(),
      executor: executor
    })
  }
  
  pub fn runner(&self) -> &QueryRunner {
    &*self.executor
  } 
  
  pub fn plugin_manager(&self) -> &PluginManager { self.executor.plugin_manager() }
  
  #[inline]
  pub fn get_type(&self, type_sign: &str) -> Result<Ty>
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