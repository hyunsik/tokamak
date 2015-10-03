extern crate common;

use std::mem;
use common::rows::{ROWBATCH_SIZE, Vector, VectorBuilder, FixedLenVectorBuilder};

#[test]
pub fn test_fixed_len_vector() {
  // building the vector
  let builder: &mut VectorBuilder = &mut FixedLenVectorBuilder::new(mem::size_of::<i32>());
  for x in 0..ROWBATCH_SIZE {
    builder.write_i32(x as i32);    
  }
  let vector: &Vector = builder.build();
  
  // verify the size
  assert_eq!(mem::size_of::<i32>() * ROWBATCH_SIZE, vector.size_in_bytes() as usize);
  
  // verify the value
  for x in 0..ROWBATCH_SIZE {
    assert_eq!(x as i32, vector.read_i32(x));
  }
}