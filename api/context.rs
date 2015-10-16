use common::err::{Error, TResult, void_ok};
use common::types::{Type, TypeId};
use common::plan::{DataSet, Plan};
use common::plugin::PackageManager;
use engine::{ExecutionPlanner, ExecutionPlan};
use sql::SQLPackage;

use df::{DataFrame};

pub struct TokamakContext 
{
  pkg_mgr: PackageManager,
  planner: ExecutionPlanner
}

impl TokamakContext 
{
  pub fn new() -> TResult<TokamakContext> 
  {
    let mut pkg_mgr = PackageManager::new_with(
      vec![
        Box::new(SQLPackage)
      ]
    );
    
    try!(pkg_mgr.load_all());
    
    Ok(TokamakContext {
      pkg_mgr: pkg_mgr,
      planner: ExecutionPlanner
    })
  }
  
  #[inline]
  pub fn get_type(&self, type_sign: &str) -> TResult<Box<Type>>
  {
    self.pkg_mgr.ty_registry().get(type_sign)
  }
  
  #[inline]
  pub fn all_types(&self) -> Vec<&str> 
  {
    self.pkg_mgr.ty_registry().all()
  }
  
  pub fn from(&self, ds: Box<DataSet>) -> DataFrame {
    DataFrame {ctx: self, plan: Plan::From(ds)}
  }
}