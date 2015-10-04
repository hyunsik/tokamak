extern crate itertools;

extern crate common;
extern crate rand;
extern crate sql;

use common::err::{Void, TResult};
use common::rows::Page;

pub mod random;

pub trait InputSource {
  fn init    (&mut self) -> Void;
  fn has_next(&mut self) -> bool;
  fn next    (&mut self) -> TResult<&Page>;
  fn close   (&mut self) -> Void;  
}