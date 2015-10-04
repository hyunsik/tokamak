extern crate common;
extern crate sql;
extern crate storage;

use std::mem;
use std::rc::Rc;

use common::types::Type;
use common::rows::{MiniPage, Page, PageBuilder, ROWBATCH_SIZE};
use sql::types::{Int4, Float4};
use storage::InputSource;
use storage::random::RandomTableGenerator;

#[test]
pub fn test_random_table() 
{
  let types: Vec<Box<Type>> = vec![
    Box::new(Int4::new()), 
    Box::new(Float4::new())
  ];
  
  let mut generator = RandomTableGenerator::new(Rc::new(types));
  
  for num in 0..2 {
    let page = generator.next().unwrap();
    assert_eq!(8192, page.bytesize());
    let m1: &MiniPage = page.minipage(0);
    let m2: &MiniPage = page.minipage(1);
    for x in 0 .. ROWBATCH_SIZE {
      println!("{}, {}", m1.read_i32(x), m2.read_f32(x));
    }
  }
}