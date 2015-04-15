use common::Column;
use common::DataType;
use common::Schema;
use common::TypeClass;
use common::constant::VECTOR_SIZE;
use intrinsics::sse;

use alloc::heap;
use std::mem;
use std::raw::Slice;


pub struct Vector {
  value_ptr: *const u8,
  size: usize,
  data_type: DataType,
  null_ptr: *const u8
}

impl Vector {

  fn value_ptr(&self) -> *const u8 {
    self.value_ptr
  }

  fn null_ptr(&self) -> *const u8 {
    self.null_ptr
  }

  fn data_type(&self) -> DataType {
    self.data_type.clone()
  }

  fn values<T>(&self) -> &mut [T] {
    let slice = Slice {data: self.value_ptr as *mut T, len: self.size};
    unsafe {
      mem::transmute(slice)
    }
  }
}


pub struct SlotVecRowBlock {
  schema: Schema,
  vectors: Vec<*mut Vector>
}

impl SlotVecRowBlock {
   fn new(schema: Schema) -> SlotVecRowBlock {
      SlotVecRowBlock {schema: schema, vectors: Vec::new()}
   }
}


pub struct AllocatedVecRowBlock {
  schema: Schema,  
  fixed_area_ptr: *mut u8,
  vectors: Vec<*mut Vector>,
}

impl AllocatedVecRowBlock {

  fn new(schema: Schema) {
    
    let mut fixed_area_size: usize = 0;    

    for c in schema.columns() {
      fixed_area_size+= 
        sse::compute_aligned_size((c.data_type.bytes_len() * VECTOR_SIZE) as usize);
    }

    unsafe {
      let fixed_area_ptr = heap::allocate(fixed_area_size, sse::ALIGNED_SIZE);
    }

    for x in 0..schema.size() {
      
    }
  }
}


trait VecRowBlockTrait {
  fn schema(&self) -> Schema;

  fn column_num(&self) -> usize;

  fn vector(&self, usize) -> *mut Vector;

  fn set_vector(&mut self, *mut Vector);
}

impl<'a> VecRowBlockTrait for SlotVecRowBlock {
    fn schema(&self) -> Schema {
      self.schema.clone()
    }

    fn column_num(&self) -> usize {
      self.schema.size()
    }

    fn vector(&self, col_id: usize) -> *mut Vector {
      self.vectors[col_id]
    }

    fn set_vector(&mut self, vec: *mut Vector) {
      self.vectors.push(vec);
    }
}

impl VecRowBlockTrait for AllocatedVecRowBlock {
    fn schema(&self) -> Schema {
        self.schema.clone()
    }

    fn column_num(&self) -> usize {
      self.schema.size()
    }

    fn vector(&self, col_id: usize) -> *mut Vector {
      self.vectors[col_id]
    }

    fn set_vector(&mut self, vec: *mut Vector) {
      self.vectors.push(vec);
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
  let rowblock = VecRowBlock {rowblock: SlotVecRowBlock::new(schema) };  

  assert_eq!(rowblock.column_num(), 3);
}