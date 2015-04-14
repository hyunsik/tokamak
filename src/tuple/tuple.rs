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

  fn column_num(&self) -> usize;
}

impl VecRowBlockTrait for SlotVecRowBlock {
    fn schema(&self) -> Schema {
      self.schema.clone()
    }

    fn column_num(&self) -> usize {
      self.schema.size()
    }
}

impl VecRowBlockTrait for AllocatedVecRowBlock {
    fn schema(&self) -> Schema {
        self.schema.clone()
    }

    fn column_num(&self) -> usize {
      self.schema.size()
    }
}

struct VecRowBlock<R> {
    rowblock: R
}

impl<R:VecRowBlockTrait> VecRowBlock<R> {
    #[inline(always)]
    fn schema(&self) -> Schema {
      self.rowblock.schema()
    }

    fn column_num(&self) -> usize {
      self.rowblock.column_num()
    }
}

#[test]
fn test_rowblock() {
  let mut columns = Vec::new();
  columns.push(Column::new("c1".to_string(), TypeClass::INT4));
  columns.push(Column::new("c2".to_string(), TypeClass::INT8));
  columns.push(Column::new("c3".to_string(), TypeClass::FLOAT4));

  let schema = Schema::new(columns);
  let rowblock = VecRowBlock {rowblock: SlotVecRowBlock {schema: schema} };  

  assert_eq!(rowblock.column_num(), 3);
}