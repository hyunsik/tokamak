use std::collections::HashMap;

use err::{Void, TResult};
use rows::Page;
use types::Type;

pub trait InputSource {
  fn open    (&mut self) -> Void;
  fn has_next(&mut self) -> bool;
  fn next    (&mut self) -> TResult<&Page>;
  fn close   (&mut self) -> Void;
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