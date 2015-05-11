use std::result;

use common::Void;
use tuple::VecRowBlockTrait;


pub trait Executor {
  fn init(&mut self) -> Void;
  fn next(&self, rowblock: &mut VecRowBlockTrait) -> Void;
  fn close(&self) -> Void;
}