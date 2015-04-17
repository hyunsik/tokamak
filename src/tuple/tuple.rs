use common::Column;
use common::Schema;
use common::data_type::*;
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
 pub fn new(schema: Schema) -> SlotVecRowBlock {
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

    let mut fixed_area_ptr = unsafe {
      heap::allocate(fixed_area_size, sse::ALIGNED_SIZE)
    };

    
    let mut vectors: Vec<Vector> = Vec::with_capacity(schema.size());
    let mut last_ptr = fixed_area_ptr as isize;

    for x in 0..schema.size() {      
      vectors.push(Vector::new(last_ptr as *const u8, schema.column(x).data_type));

      let vector_size = 
        sse::compute_aligned_size(schema.column(x).data_type.bytes_len() as usize * VECTOR_SIZE);
      last_ptr = last_ptr + (vector_size as isize);
    }

    AllocatedVecRowBlock {schema: schema, ptr: fixed_area_ptr, vectors: vectors}
  }  
}


pub trait VecRowBlockTrait {
  fn schema(&self) -> Schema;

  fn column_num(&self) -> usize;

  fn vector(&self, usize) -> &Vector;

  fn set_vector(&mut self, Vector);

  fn put_int1(&self, col_idx: usize, row_idx: usize, value: INT1_T);

  fn get_int1(&self, col_idx: usize, row_idx: usize) -> INT1_T;

  fn put_int2(&self, col_idx: usize, row_idx: usize, value: INT2_T);

  fn get_int2(&self, col_idx: usize, row_idx: usize) -> INT2_T;

  fn put_int4(&self, col_idx: usize, row_idx: usize, value: INT4_T);

  fn get_int4(&self, col_idx: usize, row_idx: usize) -> INT4_T;

  fn put_int8(&self, col_idx: usize, row_idx: usize, value: INT8_T);

  fn get_int8(&self, col_idx: usize, row_idx: usize) -> INT8_T;

  fn put_float4(&self, col_idx: usize, row_idx: usize, value: FLOAT4_T);

  fn get_float4(&self, col_idx: usize, row_idx: usize) -> FLOAT4_T;

  fn put_float8(&self, col_idx: usize, row_idx: usize, value: FLOAT8_T);

  fn get_float8(&self, col_idx: usize, row_idx: usize) -> FLOAT8_T;

  fn put_date(&self, col_idx: usize, row_idx: usize, value: DATE_T);

  fn get_date(&self, col_idx: usize, row_idx: usize) -> DATE_T;

  fn put_time(&self, col_idx: usize, row_idx: usize, value: TIME_T);

  fn get_time(&self, col_idx: usize, row_idx: usize) -> TIME_T;

  fn put_timestamp(&self, col_idx: usize, row_idx: usize, value: TIMESTAMP_T);

  fn get_timestamp(&self, col_idx: usize, row_idx: usize) -> TIMESTAMP_T;

  //fn put_text(&self, col_idx: usize, row_idx: usize, value: &String);
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

  #[inline(always)]
  fn put_int1(&self, col_idx: usize, row_idx: usize, value: INT1_T) {      
    let v : &mut [INT1_T] = self.vectors[col_idx].values();      
    unsafe{
      (*v.get_unchecked_mut(row_idx)) = value;        
    }
  }

  #[inline(always)]
  fn get_int1(&self, col_idx: usize, row_idx: usize ) -> INT1_T {      
    let v : &mut [INT1_T] = self.vectors[col_idx].values();
    unsafe {
      *v.get_unchecked(row_idx)
    }
  }

  #[inline(always)]
  fn put_int2(&self, col_idx: usize, row_idx: usize, value: INT2_T) {      
    let v : &mut [INT2_T] = self.vectors[col_idx].values();      
    unsafe{
      (*v.get_unchecked_mut(row_idx)) = value;        
    }
  }

  #[inline(always)]
  fn get_int2(&self, col_idx: usize, row_idx: usize ) -> INT2_T {      
    let v : &mut [INT2_T] = self.vectors[col_idx].values();
    unsafe {
      *v.get_unchecked(row_idx)
    }
  }

  #[inline(always)]
  fn put_int4(&self, col_idx: usize, row_idx: usize, value: INT4_T) {      
    let v : &mut [INT4_T] = self.vectors[col_idx].values();      
    unsafe{
      (*v.get_unchecked_mut(row_idx)) = value;        
    }
  }

  #[inline(always)]
  fn get_int4(&self, col_idx: usize, row_idx: usize ) -> INT4_T {      
    let v : &mut [INT4_T] = self.vectors[col_idx].values();
    unsafe {
      *v.get_unchecked(row_idx)
    }
  }

  #[inline(always)]
  fn put_int8(&self, col_idx: usize, row_idx: usize, value: INT8_T) {      
    let v : &mut [INT8_T] = self.vectors[col_idx].values();      
    unsafe{
      (*v.get_unchecked_mut(row_idx)) = value;        
    }
  }

  #[inline(always)]
  fn get_int8(&self, col_idx: usize, row_idx: usize ) -> INT8_T {      
    let v : &mut [INT8_T] = self.vectors[col_idx].values();
    unsafe {
      *v.get_unchecked(row_idx)
    }
  }

  #[inline(always)]
  fn put_float4(&self, col_idx: usize, row_idx: usize, value: FLOAT4_T) {      
    let v : &mut [FLOAT4_T] = self.vectors[col_idx].values();      
    unsafe{
      (*v.get_unchecked_mut(row_idx)) = value;        
    }
  }

  #[inline(always)]
  fn get_float4(&self, col_idx: usize, row_idx: usize ) -> FLOAT4_T {      
    let v : &mut [FLOAT4_T] = self.vectors[col_idx].values();
    unsafe {
      *v.get_unchecked(row_idx)
    }
  }

  #[inline(always)]
  fn put_float8(&self, col_idx: usize, row_idx: usize, value: FLOAT8_T) {      
    let v : &mut [FLOAT8_T] = self.vectors[col_idx].values();      
    unsafe{
      (*v.get_unchecked_mut(row_idx)) = value;        
    }
  }

  #[inline(always)]
  fn get_float8(&self, col_idx: usize, row_idx: usize ) -> FLOAT8_T {      
    let v : &mut [FLOAT8_T] = self.vectors[col_idx].values();
    unsafe {
      *v.get_unchecked(row_idx)
    }
  }

  #[inline(always)]
  fn put_date(&self, col_idx: usize, row_idx: usize, value: DATE_T) {      
    let v : &mut [DATE_T] = self.vectors[col_idx].values();      
    unsafe{
      (*v.get_unchecked_mut(row_idx)) = value;        
    }
  }

  #[inline(always)]
  fn get_date(&self, col_idx: usize, row_idx: usize ) -> DATE_T {      
    let v : &mut [DATE_T] = self.vectors[col_idx].values();
    unsafe {
      *v.get_unchecked(row_idx)
    }
  }

  fn put_time(&self, col_idx: usize, row_idx: usize, value: TIME_T) {      
    let v : &mut [TIME_T] = self.vectors[col_idx].values();      
    unsafe{
      (*v.get_unchecked_mut(row_idx)) = value;        
    }
  }

  fn get_time(&self, col_idx: usize, row_idx: usize ) -> TIME_T {      
    let v : &mut [TIME_T] = self.vectors[col_idx].values();
    unsafe {
      *v.get_unchecked(row_idx)
    }
  }

  fn put_timestamp(&self, col_idx: usize, row_idx: usize, value: TIMESTAMP_T) {      
    let v : &mut [TIMESTAMP_T] = self.vectors[col_idx].values();      
    unsafe{
      (*v.get_unchecked_mut(row_idx)) = value;        
    }
  }

  fn get_timestamp(&self, col_idx: usize, row_idx: usize ) -> TIMESTAMP_T {      
    let v : &mut [TIMESTAMP_T] = self.vectors[col_idx].values();
    unsafe {
      *v.get_unchecked(row_idx)
    }
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

  fn put_int1(&self, col_idx: usize, row_idx: usize, value: INT1_T) {      
    let v : &mut [INT1_T] = self.vectors[col_idx].values();      
    unsafe{
      (*v.get_unchecked_mut(row_idx)) = value;        
    }
  }

  fn get_int1(&self, col_idx: usize, row_idx: usize ) -> INT1_T {      
    let v : &mut [INT1_T] = self.vectors[col_idx].values();
    unsafe {
      *v.get_unchecked(row_idx)
    }
  }

  fn put_int2(&self, col_idx: usize, row_idx: usize, value: INT2_T) {      
    let v : &mut [INT2_T] = self.vectors[col_idx].values();      
    unsafe{
      (*v.get_unchecked_mut(row_idx)) = value;        
    }
  }

  fn get_int2(&self, col_idx: usize, row_idx: usize ) -> INT2_T {      
    let v : &mut [INT2_T] = self.vectors[col_idx].values();
    unsafe {
      *v.get_unchecked(row_idx)
    }
  }

  fn put_int4(&self, col_idx: usize, row_idx: usize, value: INT4_T) {      
    let v : &mut [INT4_T] = self.vectors[col_idx].values();      
    unsafe{
      (*v.get_unchecked_mut(row_idx)) = value;        
    }
  }

  fn get_int4(&self, col_idx: usize, row_idx: usize ) -> INT4_T {      
    let v : &mut [INT4_T] = self.vectors[col_idx].values();
    unsafe {
      *v.get_unchecked(row_idx)
    }
  }

  fn put_int8(&self, col_idx: usize, row_idx: usize, value: INT8_T) {      
    let v : &mut [INT8_T] = self.vectors[col_idx].values();      
    unsafe{
      (*v.get_unchecked_mut(row_idx)) = value;        
    }
  }

  fn get_int8(&self, col_idx: usize, row_idx: usize ) -> INT8_T {      
    let v : &mut [INT8_T] = self.vectors[col_idx].values();
    unsafe {
      *v.get_unchecked(row_idx)
    }
  }

  fn put_float4(&self, col_idx: usize, row_idx: usize, value: FLOAT4_T) {      
    let v : &mut [FLOAT4_T] = self.vectors[col_idx].values();      
    unsafe{
      (*v.get_unchecked_mut(row_idx)) = value;        
    }
  }

  fn get_float4(&self, col_idx: usize, row_idx: usize ) -> FLOAT4_T {      
    let v : &mut [FLOAT4_T] = self.vectors[col_idx].values();
    unsafe {
      *v.get_unchecked(row_idx)
    }
  }

  fn put_float8(&self, col_idx: usize, row_idx: usize, value: FLOAT8_T) {      
    let v : &mut [FLOAT8_T] = self.vectors[col_idx].values();      
    unsafe{
      (*v.get_unchecked_mut(row_idx)) = value;        
    }
  }

  fn get_float8(&self, col_idx: usize, row_idx: usize ) -> FLOAT8_T {      
    let v : &mut [FLOAT8_T] = self.vectors[col_idx].values();
    unsafe {
      *v.get_unchecked(row_idx)
    }
  }

  fn put_date(&self, col_idx: usize, row_idx: usize, value: DATE_T) {      
    let v : &mut [DATE_T] = self.vectors[col_idx].values();      
    unsafe{
      (*v.get_unchecked_mut(row_idx)) = value;        
    }
  }

  fn get_date(&self, col_idx: usize, row_idx: usize ) -> DATE_T {      
    let v : &mut [DATE_T] = self.vectors[col_idx].values();
    unsafe {
      *v.get_unchecked(row_idx)
    }
  }

  fn put_time(&self, col_idx: usize, row_idx: usize, value: TIME_T) {      
    let v : &mut [TIME_T] = self.vectors[col_idx].values();      
    unsafe{
      (*v.get_unchecked_mut(row_idx)) = value;        
    }
  }

  fn get_time(&self, col_idx: usize, row_idx: usize ) -> TIME_T {      
    let v : &mut [TIME_T] = self.vectors[col_idx].values();
    unsafe {
      *v.get_unchecked(row_idx)
    }
  }

  fn put_timestamp(&self, col_idx: usize, row_idx: usize, value: TIMESTAMP_T) {      
    let v : &mut [TIMESTAMP_T] = self.vectors[col_idx].values();      
    unsafe{
      (*v.get_unchecked_mut(row_idx)) = value;        
    }
  }

  fn get_timestamp(&self, col_idx: usize, row_idx: usize ) -> TIMESTAMP_T {      
    let v : &mut [TIMESTAMP_T] = self.vectors[col_idx].values();
    unsafe {
      *v.get_unchecked(row_idx)
    }
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

  #[inline(always)]
  pub fn column_num(&self) -> usize {
    self.rowblock.column_num()
  }

  #[inline(always)]
  pub fn vector(&self, col_id: usize) -> &Vector {
    self.rowblock.vector(col_id)
  }

  #[inline(always)]
  pub fn set_vector(&mut self, vec: Vector) {
    //self.rowblock.set_vector(col_id)
  }

  #[inline(always)]
  pub fn put_int1(&self, col_idx: usize, row_idx: usize, value: INT1_T) {
    self.rowblock.put_int1(col_idx, row_idx, value);
  }

  #[inline(always)]
  pub fn get_int1(&self, col_idx: usize, row_idx: usize) -> INT1_T {
    self.rowblock.get_int1(col_idx, row_idx)
  }

  #[inline(always)]
  fn put_int2(&self, col_idx: usize, row_idx: usize, value: INT2_T) {
    self.rowblock.put_int2(col_idx, row_idx, value);
  }

  #[inline(always)]
  fn get_int2(&self, col_idx: usize, row_idx: usize) -> INT2_T {
    self.rowblock.get_int2(col_idx, row_idx)
  }

  #[inline(always)]
  fn put_int4(&self, col_idx: usize, row_idx: usize, value: INT4_T) {
    self.rowblock.put_int4(col_idx, row_idx, value);
  }

  #[inline(always)]
  fn get_int4(&self, col_idx: usize, row_idx: usize) -> INT4_T {
    self.rowblock.get_int4(col_idx, row_idx)
  }

  #[inline(always)]
  fn put_int8(&self, col_idx: usize, row_idx: usize, value: INT8_T) {
    self.rowblock.put_int8(col_idx, row_idx, value);
  }

  #[inline(always)]
  fn get_int8(&self, col_idx: usize, row_idx: usize) -> INT8_T {
    self.rowblock.get_int8(col_idx, row_idx)
  }

  #[inline(always)]
  fn put_float4(&self, col_idx: usize, row_idx: usize, value: FLOAT4_T) {
    self.rowblock.put_float4(col_idx, row_idx, value);
  }

  #[inline(always)]
  fn get_float4(&self, col_idx: usize, row_idx: usize) -> FLOAT4_T {
    self.rowblock.get_float4(col_idx, row_idx)
  }

  #[inline(always)]
  fn put_float8(&self, col_idx: usize, row_idx: usize, value: FLOAT8_T) {
    self.rowblock.put_float8(col_idx, row_idx, value);
  }

  #[inline(always)]
  fn get_float8(&self, col_idx: usize, row_idx: usize) -> FLOAT8_T {
    self.rowblock.get_float8(col_idx, row_idx)
  }

  #[inline(always)]
  fn put_date(&self, col_idx: usize, row_idx: usize, value: DATE_T) {
    self.rowblock.put_date(col_idx, row_idx, value);
  }

  #[inline(always)]
  fn get_date(&self, col_idx: usize, row_idx: usize) -> DATE_T {
    self.rowblock.get_date(col_idx, row_idx)
  }

  #[inline(always)]
  fn put_time(&self, col_idx: usize, row_idx: usize, value: TIME_T) {
    self.rowblock.put_time(col_idx, row_idx, value);
  }

  #[inline(always)]
  fn get_time(&self, col_idx: usize, row_idx: usize) -> TIME_T {
    self.rowblock.get_time(col_idx, row_idx)
  }

  #[inline(always)]
  fn put_timestamp(&self, col_idx: usize, row_idx: usize, value: TIMESTAMP_T) {
    self.rowblock.put_timestamp(col_idx, row_idx, value);
  }

  #[inline(always)]
  fn get_timestamp(&self, col_idx: usize, row_idx: usize) -> TIMESTAMP_T {
    self.rowblock.get_timestamp(col_idx, row_idx)
  }
}