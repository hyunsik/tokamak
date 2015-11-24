extern crate common;

use common::session::Session;
use common::types::{i32_ty, f32_ty, i64_ty, f64_ty, Ty};
use common::rows::{MiniPage, ROWBATCH_SIZE};
use common::input::InputSource;
use common::storage::RandomTable;

macro_rules! assert_next_rows {
	($gen:expr, $num:expr) => {
		{
		  let page = $gen.next().unwrap();
		  assert_eq!($num, page.value_count());
  	}
	}
}

#[test]
pub fn test_next_once() 
{
  let types: Vec<Ty> = vec![
    i32_ty(), 
    f32_ty()
  ];
  
  let session = Session;
  let mut gen = RandomTable::new(&session, &types, 5);
  
  assert_next_rows!(gen, 5);
  assert_next_rows!(gen, 0);
}

#[test]
pub fn test_next_once2() 
{
  let types: Vec<Ty> = vec![
    i32_ty(), 
    f32_ty()
  ];
  
  let session = Session;
  let mut gen = RandomTable::new(&session, &types, ROWBATCH_SIZE);
  
  assert_next_rows!(gen, ROWBATCH_SIZE);
  assert_next_rows!(gen, 0);
}

#[test]
pub fn test_next_multiple() 
{
  let types: Vec<Ty> = vec![
    i32_ty(), 
    f32_ty()
  ];
  
  let session = Session;
  let mut gen = RandomTable::new(&session, &types, (ROWBATCH_SIZE * 2) + 100);
  
  assert_next_rows!(gen, ROWBATCH_SIZE);
  assert_next_rows!(gen, ROWBATCH_SIZE);
  assert_next_rows!(gen, 100); 
  assert_next_rows!(gen, 0);
}

#[test]
pub fn test_next_value() 
{
  let types: Vec<Ty> = vec![
    i64_ty(), 
    f64_ty()
  ];
  
  let session = Session;
  let mut gen = RandomTable::new(&session, &types, 1024);
  
  let page = gen.next().unwrap();
  
  for x in 0..page.value_count() {
  	println!("{} - {},{}", x, page.minipage(0).read_i64(x), page.minipage(1).read_f64(x));
  }
}