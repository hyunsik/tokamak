//!
//! Plugins
//!

use std::collections::BTreeMap;
use std::collections::btree_map::Entry::{Occupied, Vacant};
use std::collections::HashMap;

use err::{Error, Result, Void, void_ok};
use func::{FuncSignature, InvokeAction};
use types::{Type, TypeFactory};
use input::InputSource;

pub trait Package 
{
  fn name(&self) -> &str;

  fn types(&self) -> Vec<(&'static str, TypeFactory)>;
  
  fn funcs(&self) -> Vec<(FuncSignature, InvokeAction)>;
}

pub struct PackageManager 
{
  pkgs    : HashMap<String, Box<Package>>,
  type_registry: TypeRegistry,
  func_registry: FuncRegistry,
  src_reg : InputSourceRegistry 
}

impl PackageManager {
  pub fn new() -> PackageManager 
  {
    PackageManager {
      pkgs: HashMap::new(),
      type_registry: TypeRegistry::new(),
      func_registry: FuncRegistry::new(),
      src_reg : InputSourceRegistry::new()
    }
  }
  
  pub fn new_with(pkgs: Vec<Box<Package>>) -> PackageManager 
  {
    PackageManager {
      pkgs: pkgs.into_iter()
              .map(|p: Box<Package>| -> (String, Box<Package>) { 
                (p.name().to_string(), p) 
              })
              .collect::<HashMap<String, Box<Package>>>(),
      type_registry: TypeRegistry::new(),
      func_registry: FuncRegistry::new(),
      src_reg : InputSourceRegistry::new()
    }
  }
  
  pub fn load(&mut self, pkg: Box<Package>) -> Void
  {
    self.type_registry.add_all(pkg.types());
    self.func_registry.add_all(pkg.funcs());
    self.pkgs.insert(pkg.name().to_string(), pkg);
    
    void_ok
  }
  
  pub fn type_registry( &self) -> &TypeRegistry 
  {
    &self.type_registry    
  }
  
  pub fn func_registry(&self) -> &FuncRegistry 
  {
    &self.func_registry
  }
}

#[derive(Clone)]
pub struct FuncRegistry
{
  // key and value will be kept immutable as a just reference
  funcs: BTreeMap<FuncSignature, InvokeAction>
}

impl FuncRegistry 
{
  pub fn new() -> FuncRegistry 
  {
    FuncRegistry {
      funcs: BTreeMap::new()
    }    
  }
  
  pub fn add_all(&mut self, funcs: Vec<(FuncSignature, InvokeAction)>) -> Void {   
    for (sig, invoke) in funcs.into_iter() {
      match self.funcs.entry(sig) {
        Vacant(e)   => { 
          e.insert(invoke); 
        },
        Occupied(_) => { return Err(Error::DuplicatedFuncSign) }
      }      
    }
    
    void_ok
  }
}

#[derive(Clone)]
pub struct TypeRegistry
{
  // a base type, a function to generate type
  types: BTreeMap<String, TypeFactory>
}

impl TypeRegistry 
{
  pub fn new() -> TypeRegistry 
  {
    TypeRegistry {
      types: BTreeMap::new()
    }
  }
  
  pub fn add_all(&mut self, types: Vec<(&str, TypeFactory)>) -> Void 
  {
    for (base, factory) in types.into_iter() {
      match self.types.entry(base.to_string()) {
        Vacant(e)   => { 
          e.insert(factory); 
        },
        Occupied(_) => { return Err(Error::DuplicatedTypeId) }
      }      
    }
    
    void_ok
  }
  
  pub fn get(&self, type_sign: &str) -> Result<Box<Type>> {
    match self.types.get(type_sign) {
      Some(factory) => factory(type_sign),
      None          => Err(Error::UndefinedDataType(type_sign.to_string()))
    }
  }
  
  pub fn all(&self) -> Vec<&str> {
    self.types.keys().map(|v| &**v).collect::<Vec<&str>>()
  }
}


pub type InputSourceFactory = Box<Fn(Vec<&Type>) -> Box<InputSource>>;

pub struct InputSourceRegistry 
{
  registry: HashMap<String, InputSourceFactory>
}

impl InputSourceRegistry 
{
  pub fn new() -> InputSourceRegistry
  {
    InputSourceRegistry {
      registry: HashMap::new()      
    }
  }
}