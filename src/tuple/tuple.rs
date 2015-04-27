use bytesize::ByteSize;
use common::Schema;
use common::data_type::*;
use common::constant::VECTOR_SIZE;
use intrinsics::sse;
use memutil::Arena;
use tuple::vector::Vector;

use alloc::heap;
use std::mem;


pub struct SlotVecRowBlock<'a> {
  schema: Schema,
  vectors: Vec<Vector<'a>>,
}

impl<'a> SlotVecRowBlock<'a> {
 pub fn new(schema: Schema) -> SlotVecRowBlock<'a> {
  SlotVecRowBlock {schema: schema, vectors: Vec::new()}
  }
}


pub struct AllocatedVecRowBlock<'a> {
  schema: Schema,  
  type_lengths: Vec<u32>,
  ptr: *mut u8,
  vectors: Vec<Vector<'a>>,
  arena: Arena<'a>
}

impl<'a> AllocatedVecRowBlock<'a> {

  pub fn new(schema: Schema) -> AllocatedVecRowBlock<'a> {

    let mut fixed_area_size: usize = 0;    
    let mut type_lengths: Vec<u32> = Vec::new();

    for c in schema.columns() {
      let bytes_len = c.data_type.bytes_len();      
      type_lengths.push(bytes_len);

      fixed_area_size += 
        sse::compute_aligned_size(bytes_len as usize * VECTOR_SIZE);
    }

    let mut fixed_area_ptr = unsafe {
      heap::allocate(fixed_area_size, sse::ALIGNED_SIZE)
    };

    
    let mut vectors: Vec<Vector> = Vec::with_capacity(schema.size());
    let mut last_ptr = fixed_area_ptr as usize;

    for x in 0..schema.size() {      
      vectors.push(Vector::new(last_ptr as *const u8, schema.column(x).data_type));

      let vector_size = 
        sse::compute_aligned_size(schema.column(x).data_type.bytes_len() as usize * VECTOR_SIZE);
      last_ptr = last_ptr + vector_size;
    }

    AllocatedVecRowBlock {
      schema: schema, 
      type_lengths: type_lengths, 
      ptr: fixed_area_ptr, 
      vectors: vectors,
      arena: Arena::new(ByteSize::kb(4).as_usize())
    }
  }  
}


pub trait VecRowBlockTrait<'b> {
  fn schema(&self) -> Schema;

  fn column_num(&self) -> usize;

  fn vector(&'b self, usize) -> &Vector<'b>;

  fn set_vector(&mut self, Vector<'b>);

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

  fn put_text(&mut self, col_idx: usize, row_idx: usize, value: &str);

  fn get_text(&self, col_idx: usize, row_idx: usize) -> Option<&TEXT_T>;
}

impl<'a> VecRowBlockTrait<'a> for SlotVecRowBlock<'a> {
  fn schema(&self) -> Schema {
    self.schema.clone()
  }

  fn column_num(&self) -> usize {
    self.schema.size()
  }

  fn vector(&'a self, col_id: usize) -> &Vector<'a> {
    &self.vectors[col_id]
  }

  fn set_vector(& mut self, vec: Vector<'a>) {
    self.vectors.push(vec);
  }

  #[inline(always)]
  fn put_int1(&self, col_idx: usize, row_idx: usize, value: INT1_T) {      
    unimplemented!();
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
    unimplemented!();
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
    unimplemented!();
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
    unimplemented!();
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
    unimplemented!();
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
    unimplemented!();
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
    unimplemented!();
  }

  #[inline(always)]
  fn get_date(&self, col_idx: usize, row_idx: usize ) -> DATE_T {      
    let v : &mut [DATE_T] = self.vectors[col_idx].values();
    unsafe {
      *v.get_unchecked(row_idx)
    }
  }

  #[inline(always)]
  fn put_time(&self, col_idx: usize, row_idx: usize, value: TIME_T) {      
    unimplemented!();
  }

  #[inline(always)]
  fn get_time(&self, col_idx: usize, row_idx: usize ) -> TIME_T {      
    let v : &mut [TIME_T] = self.vectors[col_idx].values();
    unsafe {
      *v.get_unchecked(row_idx)
    }
  }

  #[inline(always)]
  fn put_timestamp(&self, col_idx: usize, row_idx: usize, value: TIMESTAMP_T) {      
    unimplemented!();
  }

  #[inline(always)]
  fn get_timestamp(&self, col_idx: usize, row_idx: usize ) -> TIMESTAMP_T {      
    let v : &mut [TIMESTAMP_T] = self.vectors[col_idx].values();
    unsafe {
      *v.get_unchecked(row_idx)
    }
  }

  fn put_text(&mut self, col_idx: usize, row_idx: usize, value: &str) {        
    unimplemented!();
  }

  fn get_text(&self, col_idx: usize, row_idx: usize) -> Option<&TEXT_T> {
    let v : &mut [TEXT_T] = self.vectors[col_idx].values();
    unsafe {
      Some(v.get_unchecked(row_idx))
    }
  }
}

impl<'a> VecRowBlockTrait<'a> for AllocatedVecRowBlock<'a> {
  fn schema(&self) -> Schema {
    self.schema.clone()
  }

  fn column_num(&self) -> usize {
    self.schema.size()
  }

  fn vector(&'a self, col_id: usize) -> &'a Vector {
    &self.vectors[col_id]
  }

  fn set_vector(&mut self, vec: Vector<'a>) {
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

  #[inline(always)]
  fn put_time(&self, col_idx: usize, row_idx: usize, value: TIME_T) {      
    let v : &mut [TIME_T] = self.vectors[col_idx].values();      
    unsafe{
      (*v.get_unchecked_mut(row_idx)) = value;        
    }
  }

  #[inline(always)]
  fn get_time(&self, col_idx: usize, row_idx: usize ) -> TIME_T {      
    let v : &mut [TIME_T] = self.vectors[col_idx].values();
    unsafe {
      *v.get_unchecked(row_idx)
    }
  }

  #[inline(always)]
  fn put_timestamp(&self, col_idx: usize, row_idx: usize, value: TIMESTAMP_T) {      
    let v : &mut [TIMESTAMP_T] = self.vectors[col_idx].values();      
    unsafe{
      (*v.get_unchecked_mut(row_idx)) = value;        
    }
  }

  #[inline(always)]
  fn get_timestamp(&self, col_idx: usize, row_idx: usize ) -> TIMESTAMP_T {      
    let v : &mut [TIMESTAMP_T] = self.vectors[col_idx].values();
    unsafe {
      *v.get_unchecked(row_idx)
    }
  }

  fn put_text(&mut self, col_idx: usize, row_idx: usize, value: &str) {
    let ptr = self.vectors[col_idx].values_ptr() as usize;    
    let mut value_ptr = ptr + (16 as usize * row_idx);

    let str_ptr = self.arena.alloc_str(value);

    let str_slice = TEXT_T::new (str_ptr, value.len() as i32);

    unsafe { 
      *(value_ptr as *mut usize) = str_ptr as usize;
    }
    value_ptr += (mem::size_of::<usize>());
    unsafe {
      *(value_ptr as *mut i32) = value.len() as i32;
    }
  }

  fn get_text(&self, col_idx: usize, row_idx: usize) -> Option<&TEXT_T> {    
    let v : &mut [TEXT_T] = self.vectors[col_idx].values();
    unsafe {
      Some(v.get_unchecked(row_idx))
    }
  }
}

pub struct VecRowBlock<R> {
  pub rowblock: R
}

impl <'a, R: VecRowBlockTrait<'a>> VecRowBlock<R> {
  #[inline(always)]
  pub fn schema(&self) -> Schema {
    self.rowblock.schema()
  }

  #[inline(always)]
  pub fn column_num(&self) -> usize {
    self.rowblock.column_num()
  }

  #[inline(always)]
  pub fn vector(&'a self, col_id: usize) -> &Vector<'a> {
    self.rowblock.vector(col_id)
  }

  #[inline(always)]
  pub fn set_vector(&mut self, vec: &Vector<'a>) {
    //self.rowblock.set_vector(col_id, vec)
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