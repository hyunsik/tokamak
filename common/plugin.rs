//!
//! Plugins
//!

use std::collections::BTreeMap;
use std::collections::btree_map::Entry;
use std::collections::btree_map::Entry::{Occupied, Vacant};
use std::collections::HashMap;

use err::{Error, TResult, Void, void_ok};
use func::{FuncSignature, InvokeAction};
use types::{Type, TypeId, TypeFactory};
use input::InputSource;

pub trait Package {
  fn name(&self) -> &str;
  fn load(
    &mut self, 
    type_reg: &mut TypeRegistry, 
    fn_reg  : &mut FuncRegistry,
    src_reg : &mut InputSourceRegistry) -> Void;
}

pub struct PackageManager {
  pkgs    : HashMap<String, Box<Package>>,
  type_reg: TypeRegistry,
  func_reg: FuncRegistry,
  src_reg : InputSourceRegistry 
}

impl PackageManager {
  pub fn new() -> PackageManager {
    PackageManager {
      pkgs: HashMap::new(),
      type_reg: TypeRegistry::new(),
      func_reg: FuncRegistry::new(),
      src_reg : InputSourceRegistry::new()
    }
  }
  
  pub fn new_with(pkgs: Vec<Box<Package>>) -> PackageManager {
    PackageManager {
      pkgs: pkgs.into_iter()
              .map(|p: Box<Package>| -> (String, Box<Package>) { 
                (p.name().to_string(), p) 
              })
              .collect::<HashMap<String, Box<Package>>>(),
      type_reg: TypeRegistry::new(),
      func_reg: FuncRegistry::new(),
      src_reg : InputSourceRegistry::new()
    }
  }
  
  pub fn add(&mut self, pkg: Box<Package>) -> &mut Self {
    // TODO - To ensure add packages before loading, we need to adopt Builder pattern. 
    self.pkgs.insert(pkg.name().to_string(), pkg);
    self
  }
  
  pub fn load_all(&mut self) -> Void {
    for (name, pkg) in self.pkgs.iter_mut() {
      try!(pkg.load(&mut self.type_reg, &mut self.func_reg, &mut self.src_reg))            
    }
    
    Ok(())
  }
  
  pub fn ty_registry(&self) -> &TypeRegistry {
    &self.type_reg    
  }
  
  pub fn fnc_registry(&self) -> &FuncRegistry {
    &self.func_reg
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
    
    void_ok()
  }
}

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
    
    void_ok()
  }
  
  pub fn get(&self, type_sign: &str) -> TResult<Box<Type>> {
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