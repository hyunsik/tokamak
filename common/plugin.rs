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
use types::{Ty, TypeFactory};
use input::InputSource;

pub trait Plugin
{
  fn name(&self) -> &str;
  
  fn load(&self, &mut PluginManager) -> Void;
}

#[derive(Clone)]
pub struct PluginManager<'a> 
{
  pkgs         : HashMap<String, Rc<Box<Plugin>>>,
  type_registry: TypeRegistry,
  func_registry: FuncRegistry,
  src_reg      : InputSourceRegistry,
  marker       : PhantomData<&'a()>  
}

impl<'a> PluginManager<'a> 
{
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
  
  #[inline]
  pub fn register_ty(&mut self, ty: (&str, TypeFactory)) -> Void
  {
  	self.type_registry.add(ty)
  }
  
  #[inline]
  pub fn register_func(&mut self, func: (FuncSignature, InvokeAction)) -> Void
  {
  	self.func_registry.add(func)
  } 
  
  #[inline]
  pub fn type_registry(&self) -> &TypeRegistry 
  {
    &self.type_registry
  }
  
  #[inline]
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
  
  #[inline]
  fn add(&mut self, func: (FuncSignature, InvokeAction)) -> Void 
	{
		match self.funcs.entry(func.0) {
      Vacant(e)   => { 
        e.insert(func.1);
        void_ok 
      },
      Occupied(_) => { return Err(Error::DuplicatedFuncSign) }
    }   
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
  
  #[inline]
  pub fn add(&mut self, ty: (&str, TypeFactory)) -> Void 
 	{
		match self.types.entry(ty.0.to_string()) {
      Vacant(e)   => { 
        e.insert(ty.1);
        void_ok 
      },
      Occupied(_) => { return Err(Error::DuplicatedTypeId) }
    }   
	}
  
  #[inline]
  pub fn get(&self, type_sign: &str) -> Result<Ty> {
    match self.types.get(type_sign) {
      Some(factory) => factory(type_sign),
      None          => Err(Error::UndefinedDataType(type_sign.to_string()))
    }
  }
  
  #[inline]
  pub fn all(&self) -> Vec<&str> {
    self.types.keys().map(|v| &**v).collect::<Vec<&str>>()
  }
}


pub type InputSourceFactory = Rc<Fn(Vec<&Ty>) -> Box<InputSource>>;

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