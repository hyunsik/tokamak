pub use self::text_scanner::DelimTextScanner;

mod text_scanner;
pub mod text_splitter;

use std::result;

use common::Void;
use schema::Schema;
use eval::MapEval;
use rows::RowBlock;

pub trait Executor {
  fn init(&mut self) -> Void;
  fn next(&mut self, rowblock: &mut RowBlock) -> Void;
  fn close(&mut self) -> Void;

  fn in_schema(&self) -> &Schema;
  fn out_schema(&self) -> &Schema;
}

pub struct Projection {
  evals: Vec<Box<MapEval>>,

  in_schema: Schema,
  out_schema: Schema
}

impl Projection {
  pub fn new(in_schema: Schema, 
             out_schema: Schema, 
             evals: Vec<Box<MapEval>>) -> Projection {
    Projection {
      evals: evals,
      in_schema: in_schema,
      out_schema: out_schema
    }
  }
}