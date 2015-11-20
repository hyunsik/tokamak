extern crate common;

use common::session::Session;
use common::types::{i32_ty, f32_ty, Ty};
use common::rows::{ROWBATCH_SIZE};
use common::input::InputSource;
use common::storage::{MemTable, RandomTable};

macro_rules! assert_write_rows {
	($gen:expr, $mem:expr, $num:expr, $total_row:expr) => {
		{
		  let page = $gen.next().unwrap();
		  assert_eq!($num, page.value_count());
		  $mem.write(page).ok().unwrap();
		  assert_eq!($total_row, $mem.row_num());
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
  let mut mem = MemTable::new(&session, &types, &vec!["x","y"]);
  
  assert_write_rows!(gen, mem, 5, 5);
  assert_write_rows!(gen, mem, 0, 5);
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
  let mut mem = MemTable::new(&session, &types, &vec!["x","y"]);
  
  assert_write_rows!(gen, mem, ROWBATCH_SIZE, ROWBATCH_SIZE);
  assert_write_rows!(gen, mem, 0,             ROWBATCH_SIZE);
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
  let mut mem = MemTable::new(&session, &types, &vec!["x","y"]);
  
  assert_write_rows!(gen, mem, ROWBATCH_SIZE, ROWBATCH_SIZE);
  assert_write_rows!(gen, mem, ROWBATCH_SIZE, ROWBATCH_SIZE * 2);
  assert_write_rows!(gen, mem, 100,           ROWBATCH_SIZE * 2 + 100); 
  assert_write_rows!(gen, mem, 0,             ROWBATCH_SIZE * 2 + 100);
}

#[test]
pub fn test_read() 
{
  let types: Vec<Ty> = vec![
    i32_ty(), 
    f32_ty()
  ];
  
  let session = Session;
  let mut gen = RandomTable::new(&session, &types, 5);
  let mut mem = MemTable::new(&session, &types, &vec!["x","y"]);
  
  assert_write_rows!(gen, mem, 5, 5);
  assert_write_rows!(gen, mem, 0, 5);
  
  let reader = mem.reader();
  for x in reader {
  	let r: (i32, f32) = x.ok().unwrap();
  }
}