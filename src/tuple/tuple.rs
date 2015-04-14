use common::Column;
use common::Schema;
use common::TypeClass;

pub struct SlotVecRowBlock {
  schema: Schema
}

pub struct AllocatedVecRowBlock {
  schema: Schema
}

trait VecRowBlockTrait {
  fn schema(&self) -> Schema;
}

impl VecRowBlockTrait for SlotVecRowBlock {
    fn schema(&self) -> Schema {
        self.schema.clone()
    }
}

impl VecRowBlockTrait for AllocatedVecRowBlock {
    fn schema(&self) -> Schema {
        self.schema.clone()
    }
}

struct RowBlock<R> {
    rowblock: R
}

impl<R:VecRowBlockTrait> RowBlock<R> {
    #[inline(always)]
    fn schema(&self) -> Schema {
      self.rowblock.schema()
    }
}

#[test]
fn test_rowblock() {
  let mut columns = Vec::new();
  columns.push(Column::new("c1".to_string(), TypeClass::INT4));
  columns.push(Column::new("c2".to_string(), TypeClass::INT8));
  columns.push(Column::new("c3".to_string(), TypeClass::FLOAT4));

  let schema = Schema::new(columns);
  let rowblock = RowBlock {rowblock: SlotVecRowBlock {schema: schema} };
  //let rowblock.schema();

  //assert!(schema == rowblock.schema());
}