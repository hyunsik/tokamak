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
pub use self::vrows::{HeapVRowBlock, PtrVector, BorrowedVRowBlock};

pub mod vector;
pub use self::vector::{Vector};

use schema::Schema;
use types::*;

struct VectorDesc {
  repeating: bool,
  nullable: bool,
  sorted: bool,
  contiguous: bool
}

pub trait RowBlockWriter : RowBlock {
  fn put_int1(&mut self, row_id: usize, col_id: usize, value: INT1);

  fn put_int2(&mut self, row_id: usize, col_id: usize, value: INT2);

  fn put_int4(&mut self, row_id: usize, col_id: usize, value: INT4);

  fn put_int8(&mut self, row_id: usize, col_id: usize, value: INT8);

  fn put_float4(&mut self, row_id: usize, col_id: usize, value: FLOAT4);

  fn put_float8(&mut self, row_id: usize, col_id: usize, value: FLOAT8);

  fn put_date(&mut self, row_id: usize, col_id: usize, value: DATE);

  fn put_time(&mut self, row_id: usize, col_id: usize, value: TIME);

  fn put_timestamp(&mut self, row_id: usize, col_id: usize, value: TIMESTAMP);

  fn put_text(&mut self, row_id: usize, col_id: usize, value: &TEXT);
  
  fn put_text_from_str(&mut self, row_id: usize, col_id: usize, value: &str);
}

pub trait AsRowBlock {
  fn as_reader(&self) -> &RowBlock;
}

pub trait RowBlock : AsRowBlock {
  fn column_num(&self) -> usize;

  fn schema(&self) -> &Schema;  

  fn vector(&self, col_idx: usize) -> &Vector;

  fn selected(&self) -> &Vec<bool>;

  fn selected_mut(&mut self) -> &mut Vec<bool>;
  
  fn row_num(&self) -> usize;
  
  fn set_row_num(&mut self, row_num: usize);

  fn get_int1(&self, row_id: usize, col_id: usize) -> INT1;  

  fn get_int2(&self, row_id: usize, col_id: usize) -> INT2;  

  fn get_int4(&self, row_id: usize, col_id: usize) -> INT4;  

  fn get_int8(&self, row_id: usize, col_id: usize) -> INT8;  

  fn get_float4(&self, row_id: usize, col_id: usize) -> FLOAT4;  

  fn get_float8(&self, row_id: usize, col_id: usize) -> FLOAT8;  

  fn get_date(&self, row_id: usize, col_id: usize) -> DATE;  

  fn get_time(&self, row_id: usize, col_id: usize) -> TIME;  

  fn get_timestamp(&self, row_id: usize, col_id: usize) -> TIMESTAMP;  

  fn get_text(&self, row_id: usize, col_id: usize) -> &TEXT;
}

#[inline]
pub fn copy_vectors<'a>(src: &'a RowBlock, dest: &'a mut BorrowedVRowBlock<'a>) {
  debug_assert!(src.column_num() == dest.column_num(),
    "source and destination rowblocks must have the same width");
    
  for x in 0..src.column_num() {
    dest.set_vector(x, src.vector(x));
  }
  
  dest.set_row_num(src.row_num());
}