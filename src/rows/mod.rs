pub mod vectorized_rows;
pub use self::vectorized_rows::{AllocatedVecRowBlock, SlotVecRowBlock};

mod vector;
pub use self::vector::Vector;

use common::Schema;
use common::data_type::*;

pub struct RowBlock<R> {
  pub rowblock: R
}

impl <'a, R: RowBlockTrait<'a>> RowBlockTrait<'a> for RowBlock<R> {
  #[inline]
  fn schema(&'a self) -> &'a Schema {
    self.rowblock.schema()
  }

  #[inline]
  fn column_num(&self) -> usize {
    self.rowblock.column_num()
  }

  #[inline]
  fn vector(&'a self, col_id: usize) -> &Vector<'a> {
    self.rowblock.vector(col_id)
  }

  #[inline]
  fn set_vector(&'a mut self, vec: &'a Vector<'a>) {
    self.rowblock.set_vector(vec)
  }

  #[inline]
  fn put_int1(&self, col_idx: usize, row_idx: usize, value: INT1_T) {
    self.rowblock.put_int1(col_idx, row_idx, value);
  }

  #[inline]
  fn get_int1(&self, col_idx: usize, row_idx: usize) -> INT1_T {
    self.rowblock.get_int1(col_idx, row_idx)
  }

  #[inline]
  fn put_int2(&self, col_idx: usize, row_idx: usize, value: INT2_T) {
    self.rowblock.put_int2(col_idx, row_idx, value);
  }

  #[inline]
  fn get_int2(&self, col_idx: usize, row_idx: usize) -> INT2_T {
    self.rowblock.get_int2(col_idx, row_idx)
  }

  #[inline]
  fn put_int4(&self, col_idx: usize, row_idx: usize, value: INT4_T) {
    self.rowblock.put_int4(col_idx, row_idx, value);
  }

  #[inline]
  fn get_int4(&self, col_idx: usize, row_idx: usize) -> INT4_T {
    self.rowblock.get_int4(col_idx, row_idx)
  }

  #[inline]
  fn put_int8(&self, col_idx: usize, row_idx: usize, value: INT8_T) {
    self.rowblock.put_int8(col_idx, row_idx, value);
  }

  #[inline]
  fn get_int8(&self, col_idx: usize, row_idx: usize) -> INT8_T {
    self.rowblock.get_int8(col_idx, row_idx)
  }

  #[inline]
  fn put_float4(&self, col_idx: usize, row_idx: usize, value: FLOAT4_T) {
    self.rowblock.put_float4(col_idx, row_idx, value);
  }

  #[inline]
  fn get_float4(&self, col_idx: usize, row_idx: usize) -> FLOAT4_T {
    self.rowblock.get_float4(col_idx, row_idx)
  }

  #[inline]
  fn put_float8(&self, col_idx: usize, row_idx: usize, value: FLOAT8_T) {
    self.rowblock.put_float8(col_idx, row_idx, value);
  }

  #[inline]
  fn get_float8(&self, col_idx: usize, row_idx: usize) -> FLOAT8_T {
    self.rowblock.get_float8(col_idx, row_idx)
  }

  #[inline]
  fn put_date(&self, col_idx: usize, row_idx: usize, value: DATE_T) {
    self.rowblock.put_date(col_idx, row_idx, value);
  }

  #[inline]
  fn get_date(&self, col_idx: usize, row_idx: usize) -> DATE_T {
    self.rowblock.get_date(col_idx, row_idx)
  }

  #[inline]
  fn put_time(&self, col_idx: usize, row_idx: usize, value: TIME_T) {
    self.rowblock.put_time(col_idx, row_idx, value);
  }

  #[inline]
  fn get_time(&self, col_idx: usize, row_idx: usize) -> TIME_T {
    self.rowblock.get_time(col_idx, row_idx)
  }

  #[inline]
  fn put_timestamp(&self, col_idx: usize, row_idx: usize, value: TIMESTAMP_T) {
    self.rowblock.put_timestamp(col_idx, row_idx, value);
  }

  #[inline]
  fn get_timestamp(&self, col_idx: usize, row_idx: usize) -> TIMESTAMP_T {
    self.rowblock.get_timestamp(col_idx, row_idx)
  }

  fn put_text(&mut self, col_idx: usize, row_idx: usize, value: &str) {
    self.rowblock.put_text(col_idx, row_idx, value);
  }

  #[inline]
  fn get_text(&self, col_idx: usize, row_idx: usize) -> &TEXT_T {
    self.rowblock.get_text(col_idx, row_idx)
  }
}

pub trait RowBlockTrait<'b> {
  fn schema(&'b self) -> &'b Schema;

  fn column_num(&self) -> usize;

  fn vector(&'b self, usize) -> &Vector<'b>;

  fn set_vector(&'b mut self, &'b Vector<'b>);

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

  fn get_text(&self, col_idx: usize, row_idx: usize) -> &TEXT_T;
}