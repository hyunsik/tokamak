use std::result;

use common::Error;
use tuple::VecRowBlockTrait;


pub trait Executor {
  fn init(&self) -> Result<bool, Error>;
  fn next(&self, rowblock: &mut VecRowBlockTrait) -> Result<bool, Error>;
  fn close(&self) -> Result<bool, Error>;
}