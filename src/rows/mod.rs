//!
//! In-memory Row block representation implementation
//!
//! In theoretical, a row block represents contains a chunk of a relational 
//! table or a slice of a matrix. In practice context, a row block contains 
//! a list of tuples. We designed that its actual in-memory representation 
//! can be varied according to input tables and execution plan.
//!
//! `RowBlock` is a trait for all row block representations. We have roughtly 
//! two kinds of row block implementations:
//!
//! * Row-oriented row block: all fields in a row are sequentially stored 
//! in adjacent memory.
//! * Column-oriented row block: column values for each column are 
//! sequentially stored in adjacent memory.
//!
//! Its design considerations are as follows:
//!
//! * RowBlock should contain a chunk of a relational table as well as a 
//! matrix of linear algebra
//! * Each column vector, a part of a `RowBlock`, can be different 
//! encoding, different compression, and different memory representation.

pub mod vrows;
pub use self::vrows::{AllocatedVecRowBlock, PtrVector, SlotVecRowBlock};

pub mod vector;
pub use self::vector::{Vector};

pub mod vectest;

use schema::Schema;
use types::*;

struct VectorDesc {
  repeating: bool,
  nullable: bool,
  sorted: bool,
  contiguous: bool
}

pub trait RowBlock<'b> {
  fn column_num(&self) -> usize;

  fn schema(&'b self) -> &'b Schema;  

  fn vector(&'b self, usize) -> &PtrVector<'b>;

  fn set_vector(&'b mut self, &'b PtrVector<'b>);

  fn put_int1(&mut self, col_idx: usize, row_idx: usize, value: INT1_T);

  fn get_int1(&self, col_idx: usize, row_idx: usize) -> INT1_T;

  fn put_int2(&mut self, col_idx: usize, row_idx: usize, value: INT2_T);

  fn get_int2(&self, col_idx: usize, row_idx: usize) -> INT2_T;

  fn put_int4(&mut self, col_idx: usize, row_idx: usize, value: INT4_T);

  fn get_int4(&self, col_idx: usize, row_idx: usize) -> INT4_T;

  fn put_int8(&mut self, col_idx: usize, row_idx: usize, value: INT8_T);

  fn get_int8(&self, col_idx: usize, row_idx: usize) -> INT8_T;

  fn put_float4(&mut self, col_idx: usize, row_idx: usize, value: FLOAT4_T);

  fn get_float4(&self, col_idx: usize, row_idx: usize) -> FLOAT4_T;

  fn put_float8(&mut self, col_idx: usize, row_idx: usize, value: FLOAT8_T);

  fn get_float8(&self, col_idx: usize, row_idx: usize) -> FLOAT8_T;

  fn put_date(&mut self, col_idx: usize, row_idx: usize, value: DATE_T);

  fn get_date(&self, col_idx: usize, row_idx: usize) -> DATE_T;

  fn put_time(&mut self, col_idx: usize, row_idx: usize, value: TIME_T);

  fn get_time(&self, col_idx: usize, row_idx: usize) -> TIME_T;

  fn put_timestamp(&mut self, col_idx: usize, row_idx: usize, value: TIMESTAMP_T);

  fn get_timestamp(&self, col_idx: usize, row_idx: usize) -> TIMESTAMP_T;

  fn put_text(&mut self, col_idx: usize, row_idx: usize, value: &str);

  fn get_text(&self, col_idx: usize, row_idx: usize) -> &TEXT_T;
}