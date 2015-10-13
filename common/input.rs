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