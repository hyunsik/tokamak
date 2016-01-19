use err::{Void, Result};
use page::Page;

pub trait InputSource {
  fn open    (&mut self) -> Void;
  fn has_next(&mut self) -> bool;
  fn next    (&mut self) -> Result<&Page>;
  fn close   (&mut self) -> Void;
}

pub struct InputSourceFactory;

pub fn get_factory(name: &str) -> InputSourceFactory {
  InputSourceFactory
}