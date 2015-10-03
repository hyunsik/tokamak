/// This row implementation is basically based on Pax, 
/// but it has a variation in terms of variable-length blocks.
///
/// ## Deign Consideration
///
/// The API is being designed with the following design consideration:
/// 
/// * Reuseable allocated memory
/// * Vectorized processing
/// * Various encodings (light/heavy weight compression and no 
///   deserialization from storage pages)
/// * Late materialization (Refer to [3])
///
/// ## References
/// * [1] Daniel Abadi et al., The Design and Implementation of Modern Column-Oriented Database 
///       Systems
/// * [2] 
/// * [3] Daniel J. Abadi ea al., Materialization Strategies in a Column-Oriented DBMS, ICDE 2007

use alloc::heap;
use std::marker;
use std::slice;

use types::Type;
use platform::{CACHE_LINE_SIZE, get_aligned_size};

/// Each executor and operator process a batch of rows at a time for better throughput.
/// The experiment of MonetDB presented that 1024 is the best number of a row batch.
/// It's reason why I currently use 1024 as the number of row batch. 
pub static ROWBATCH_SIZE: usize = 1024; 

/// Type for column index
pub type ColumnId = usize;
/// Type for row position
pub type RowId = usize;

pub struct VTupleBatch 
{
  vectors: Vec<Box<Vector>>,   // vectors
  len : RowId,                 // number of rows 
  selected: Option<Vec<RowId>> // selection positions
}

impl VTupleBatch 
{
  fn vector(&self, cid: ColumnId) -> &Vector 
  {
    debug_assert!(cid < self.width()); 
    &*self.vectors[cid] 
  }
  
  fn width(&self) -> usize { self.vectors.len() }
  
  fn len(&self) -> usize { self.len }
}

pub trait Vector 
{
  fn bytesize(&self) -> u32;
  
  fn read_i8(&self, pos: RowId) -> i8;
  
  fn read_i16(&self, pos: RowId) -> i16;
  
  fn read_i32(&self, pos: RowId) -> i32;
  
  fn read_i64(&self, pos: RowId) -> i64;
  
  fn read_f32(&self, pos: RowId) -> f32;
  
  fn read_f64(&self, pos: RowId) -> f64;
}

/// Writer for Vector. The writer internally must have a cursor to write a value.
/// For each write, the cursor must move forward the cursor.   
/// You must call finalize() before reading any value from the Vector.  
pub trait VectorWriter {
  fn write_i8(&mut self, v: i8);
  
  fn write_i16(&mut self, v: i16);
  
  fn write_i32(&mut self, v: i32);
  
  fn write_i64(&mut self, v: i64);
  
  fn write_f32(&mut self, v: f32);
  
  fn write_f64(&mut self, v: f64);
  
  fn write_bytes(&mut self, v: &[u8]);
  
  fn reset(&mut self);
  
  fn finalize(&mut self);
}

struct VTupleBatchBuilder {
 
}

/*
pub struct PageBuilder {
  batch: VTupleBatch    
}

impl PageBuilder 
{
  pub fn new(types: &Vec<Box<Type>>) -> PageBuilder {
    
    let vectors = types
      .iter()
      .map(|ty| {
        ty.create_vector()
      })
      .collect::<Vec<Box<Vector>>>();
    
    PageBuilder {
      batch: VTupleBatch { 
        vectors : vectors,
        len     : 0,
        selected: None
      }
    }
  }
}

impl PageBuilder {
  fn writer(&self, cid: ColumnId) -> &VectorWriter {
    &*self.page.vectors[cid as usize].writer()
  }
  
  fn build(&mut self) -> &mut Page {
    for v in self.page.vectors {
      v.writer().finalize();
    }
    &self.page     
  }
}*/