use common::err::TResult;
use common::types::{Type, TypeId};
use common::plugin::PackageManager;
use sql::SQLPackage;

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
  pub fn get_type(&self, sig: &TypeId) -> Option<&Type>
  {
    self.pkg_mgr.ty_registry().get(sig)
  } 
}