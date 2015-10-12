use common::err::{Error, TResult, void_ok};
use common::types::{Type, TypeId};
use common::plugin::PackageManager;
use sql::SQLPackage;

use df::{DataSet, DataFrame};

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
  
  #[inline]
  pub fn get_type(&self, ty_id: &str) -> Option<&Type>
  {
    self.pkg_mgr.ty_registry().get(&TypeId{base: ty_id.to_string()})
  }
  
  #[inline]
  pub fn all_types(&self) -> Vec<&Type> 
  {
    self.pkg_mgr.ty_registry().all()
  }
}