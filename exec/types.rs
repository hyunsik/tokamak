use std::collections::BTreeMap;

use common::types::{Type, TypeRegistry};
use sql::types::{Int4Type, Float4Type};

pub struct TypeRegistry 
{
  types: BTreeMap<TypeId, Box<Type>>
}

impl TypeRegistry 
{
  fn new(types: Vec<Box<Type>>) -> TypeRegistry 
  {
    TypeRegistry {
      types: types.into_iter()
        .map(|ty: Box<Type>| { 
            return (*ty.id(), ty) 
        })
        .collect::<BTreeMap<TypeId, Box<Type>>>()    
    }
  }
  
  fn default() -> TypeRegistry {
    let standard_types = vec![
      Box::new(Int4Type) as Box<Type>,
      Box::new(Float4Type) as Box<Type>
    ];
    TypeRegistry::new(standard_types)
  }
}

impl TypeManager for TypeRegistry {
  fn get(&self, id: &TypeId) -> Option<&Type> {
    match self.types.get(id) {
      Some(x) => Some(&**x),
      None    => None
    }
  }
  
  fn types(&self) -> Vec<&Type> {
    self.types.values().map(|t| &**t).collect::<Vec<&Type>>()
  }
}