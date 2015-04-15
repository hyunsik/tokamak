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
  ptr: *const u8,
  data_type: DataType,

}

impl Vector {
  fn new(ptr: *const u8, data_type: DataType) -> Vector {
    Vector {ptr: ptr, data_type: data_type}
  }

  fn value_ptr(&self) -> *const u8 {
    self.ptr
  }

  fn data_type(&self) -> DataType {
    self.data_type.clone()
  }

  fn values<T>(&self) -> &mut [T] {
    let slice = Slice {data: self.ptr as *mut T, len: VECTOR_SIZE};
    unsafe {
      mem::transmute(slice)
    }
  }
}


pub struct SlotVecRowBlock {
  schema: Schema,
  vectors: Vec<Vector>
}

impl SlotVecRowBlock {
   fn new(schema: Schema) -> SlotVecRowBlock {
      SlotVecRowBlock {schema: schema, vectors: Vec::new()}
   }
}


pub struct AllocatedVecRowBlock {
  schema: Schema,  
  ptr: *mut u8,
  vectors: Vec<Vector>,
}

impl AllocatedVecRowBlock {

  pub fn new(schema: Schema) -> AllocatedVecRowBlock {
    
    let mut fixed_area_size: usize = 0;    

    for c in schema.columns() {
      fixed_area_size += 
        sse::compute_aligned_size(c.data_type.bytes_len() as usize * VECTOR_SIZE);
    }

    let mut fixed_area_ptr;
    unsafe {
      fixed_area_ptr = heap::allocate(fixed_area_size, sse::ALIGNED_SIZE);
    }

    
    let mut vectors: Vec<Vector> = Vec::with_capacity(schema.size());
    let mut ptr = fixed_area_ptr;
    for x in 0..schema.size() {      
      vectors.push(Vector::new(ptr, schema.column(x).data_type));
    }

    AllocatedVecRowBlock {schema: schema, ptr: ptr, vectors: vectors}
  }
}


trait VecRowBlockTrait {
  fn schema(&self) -> Schema;

  fn column_num(&self) -> usize;

  fn vector(&self, usize) -> &Vector;

  fn set_vector(&mut self, Vector);

  fn put_int1(&self, col_idx: usize, row_idx: usize, value: i8);

  fn get_int1(&self, col_idx: usize, row_idx: usize) -> i8;

  fn put_text(&self, col_idx: usize, row_idx: usize, value: &String);
}

impl VecRowBlockTrait for SlotVecRowBlock {
    fn schema(&self) -> Schema {
      self.schema.clone()
    }

    fn column_num(&self) -> usize {
      self.schema.size()
    }

    fn vector(&self, col_id: usize) -> &Vector {
      &self.vectors[col_id]
    }

    fn set_vector(&mut self, vec: Vector) {
      self.vectors.push(vec);
    }

    fn put_int1(&self, col_idx: usize, row_idx: usize, value: i8) {
      panic!("");    
    }

    fn get_int1(&self, col_idx: usize, row_idx: usize) -> i8 {
      panic!("");
    }

    fn put_text(&self, col_idx: usize, row_idx: usize, value: &String) {
      panic!("put_text");
    }
}

impl VecRowBlockTrait for AllocatedVecRowBlock {
    fn schema(&self) -> Schema {
        self.schema.clone()
    }

    fn column_num(&self) -> usize {
      self.schema.size()
    }

    fn vector(&self, col_id: usize) -> &Vector {
      &self.vectors[col_id]
    }

    fn set_vector(&mut self, vec: Vector) {
      self.vectors.push(vec);
    }

    fn put_int1(&self, col_idx: usize, row_idx: usize, value: i8) {      
      let v : &mut [i8] = self.vectors[col_idx].values();
      v[row_idx] = value;
    }

    fn get_int1(&self, col_idx: usize, row_idx: usize ) -> i8 {      
      let v : &mut [i8] = self.vectors[col_idx].values();
      v[row_idx]
    }

    fn put_text(&self, col_idx: usize, row_idx: usize, value: &String) {
      
    }
}

pub struct VecRowBlock<R> {
    pub rowblock: R
}

impl<R:VecRowBlockTrait> VecRowBlock<R> {
    #[inline(always)]
    pub fn schema(&self) -> Schema {
      self.rowblock.schema()
    }

    pub fn column_num(&self) -> usize {
      self.rowblock.column_num()
    }

    pub fn vector(&self, col_id: usize) -> &Vector {
      self.rowblock.vector(col_id)
    }

    pub fn set_vector(&mut self, vec: Vector) {
    }

    pub fn put_int1(&self, col_idx: usize, row_idx: usize, value: i8) {
      self.rowblock.put_int1(col_idx, row_idx, value);
    }

    pub fn get_int1(&self, col_idx: usize, row_idx: usize) -> i8 {
      self.rowblock.get_int1(col_idx, row_idx)
    }

    fn put_text(&self, col_idx: usize, row_idx: usize, value: &String) {
      self.rowblock.put_text(col_idx, row_idx, value);
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