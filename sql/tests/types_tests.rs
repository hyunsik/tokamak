extern crate common;
extern crate sql;

use std::mem;
use common::types::Type;
use sql::types::{Int4, Float4};
use common::rows::{Page, PageBuilder, ROWBATCH_SIZE};

#[test]
pub fn test_fixed_len_vector() {
  
  let types: Vec<Box<Type>> = vec![
    Box::new(Int4::new()), 
    Box::new(Float4::new())
  ];
  
  let mut builder = PageBuilder::new(&types);

  {
    let mut writer1 = builder.writer(0);
    for x in 0..ROWBATCH_SIZE {
      writer1.write_i32(x as i32);    
    }
  }
  
  {
    let mut writer2 = builder.writer(1);
    for x in 0..ROWBATCH_SIZE {
      writer2.write_f32(x as f32);    
    }
  }
  
  let page: &Page = builder.build();
  
  // verify the size
//  assert_eq!(mem::size_of::<i32>() * ROWBATCH_SIZE, vector.size_in_bytes() as usize);
  
  // verify the value
  for x in 0..ROWBATCH_SIZE {
    assert_eq!(x as i32, page.minipage(0).read_i32(x));
  }
  
  // verify the value
  for x in 0..ROWBATCH_SIZE {
    assert_eq!(x as f32, page.minipage(1).read_f32(x));
  }
}