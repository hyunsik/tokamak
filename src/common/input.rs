use err::{Void, Result};
use page::Page;

pub trait InputSource {
  fn open    (&mut self) -> Void;
  fn has_next(&mut self) -> bool;
  fn next    (&mut self) -> Result<&Page>;
  fn close   (&mut self) -> Void;
}