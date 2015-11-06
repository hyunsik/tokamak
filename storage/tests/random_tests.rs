extern crate common;
extern crate sql;
extern crate storage;

use common::session::Session;
use common::types::{i32_ty, f32_ty, Ty};
use common::rows::{MiniPage, ROWBATCH_SIZE};
use common::input::InputSource;
use storage::RandomTableGenerator;

#[test]
pub fn test_random_table() 
{
  let types: Vec<Ty> = vec![
    i32_ty(), 
    f32_ty()
  ];
  
  let session = Session;
  let mut generator = RandomTableGenerator::new(&session, &types, 5);
  
  for _ in 0..2 {
    let page = generator.next().unwrap();
    assert_eq!(8192, page.bytesize());
    let m1: &MiniPage = page.minipage(0);
    let m2: &MiniPage = page.minipage(1);
    for x in 0 .. ROWBATCH_SIZE {
      println!("{}, {}", m1.read_i32(x), m2.read_f32(x));
    }
  }
}