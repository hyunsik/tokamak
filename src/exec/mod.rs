pub use self::text_scanner::DelimTextScanner;

mod text_scanner;
pub mod text_splitter;

use std::result;
use common::Void;
use rows::RowBlock;

pub trait Executor {
  fn init(&mut self) -> Void;
  fn next(&self, rowblock: &mut RowBlock) -> Void;
  fn close(&self) -> Void;
}