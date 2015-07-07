use common::err::{Error, TResult, Void};
use rows::RowBlock;
use rows::vector::Vector1;
use common::types::DataType;
use common::schema::Schema;

trait Eval {
  fn datatype(&self) -> &DataType;
  fn bind(&self, schema: &Schema) -> Void;
  fn eval(&self, RowBlock) -> TResult<&Vector1>;  
  fn is_constant(&self) -> bool;
}