use common::Schema;

trait RowBatch {
  fn schema(&self) -> Schema;
}