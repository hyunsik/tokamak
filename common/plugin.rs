//!
//! Plugins
//!

use std::collections::BTreeMap;
use std::collections::btree_map::Entry::{Occupied, Vacant};
use std::collections::HashMap;
use std::marker::PhantomData;
use std::cell::RefCell;
use std::rc::Rc;

use err::{Error, Result, Void, void_ok};
use func::{FuncSignature, InvokeAction};
use types::{Type, TypeFactory};
use input::InputSource;

pub trait Plugin
{
  fn name(&self) -> &str;

  fn types(&self) -> Vec<(&'static str, TypeFactory)>;
  
  fn funcs(&self) -> Vec<(FuncSignature, InvokeAction)>;
}

#[derive(Clone)]
pub struct PluginManager<'a> 
{
  pkgs    : HashMap<String, Rc<Box<Plugin>>>,
  type_registry: TypeRegistry,
  func_registry: FuncRegistry,
  src_reg      : InputSourceRegistry,
  marker       : PhantomData<&'a()>  
}

impl<'a> PluginManager<'a> {
  pub fn new() -> PluginManager<'a> 
  {
    PluginManager {
      pkgs: HashMap::new(),
      type_registry: TypeRegistry::new(),
      func_registry: FuncRegistry::new(),
      src_reg : InputSourceRegistry::new(),
      marker  : PhantomData
    }
  }
  
  pub fn load(&mut self, plugin: Box<Plugin>) -> Void
  {
    println!("Enter Plugin::load()");
    self.type_registry.add_all(plugin.types());
    println!("types are all loaded");
    self.func_registry.add_all(plugin.funcs());
    println!("funcs are all loaded");
    self.pkgs.insert(plugin.name().to_string(), Rc::new(plugin));
    
    void_ok
  }
  
  pub fn type_registry(&self) -> &TypeRegistry 
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


pub type InputSourceFactory = Rc<Fn(Vec<&Type>) -> Box<InputSource>>;

#[derive(Clone)]
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