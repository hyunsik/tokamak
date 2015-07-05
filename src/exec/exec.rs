use std::result;

use common::Void;
use rows::RowBlockTrait;

pub trait Executor {
  fn init(&mut self) -> Void;
  fn next(&self, rowblock: &mut RowBlockTrait) -> Void;
  fn close(&self) -> Void;
}