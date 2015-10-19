use common::err::TResult;
use common::types::Type;
use common::plan::{DataSet, Plan, PlanContext};
use common::plugin::{PackageManager, TypeRegistry, FuncRegistry};
use sql::SQLPackage;

use df::{DataFrame};

pub struct TokamakContext 
{
  pkg_mgr: PackageManager
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
      pkg_mgr: pkg_mgr
    })
  }
  
  pub fn package_manager(&self) -> &PackageManager { &self.pkg_mgr }
  
  #[inline]
  pub fn get_type(&self, type_sign: &str) -> TResult<Box<Type>>
  {
    self.pkg_mgr.type_registry().get(type_sign)
  }
  
  #[inline]
  pub fn all_types(&self) -> Vec<&str> 
  {
    self.pkg_mgr.type_registry().all()
  }
  
  pub fn from(&self, ds: Box<DataSet>) -> DataFrame {
    DataFrame {ctx: self, plan: Plan::From(ds)}
  }
}

impl PlanContext for TokamakContext 
{
  fn type_registry(&self) -> &TypeRegistry 
  {
    self.pkg_mgr.type_registry()
  }
  
  fn func_registry(&self) -> &FuncRegistry 
  {
    self.pkg_mgr.func_registry()
  }
}