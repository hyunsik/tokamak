extern crate common;

use common::session::Session;
use common::types::{i32_ty, f32_ty, Ty};
use common::rows::{Page, ROWBATCH_SIZE};
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

#[test]
pub fn test_write_projected() 
{
  let types: Vec<Ty> = vec![
    i32_ty(), 
    f32_ty(),
    i32_ty(),
  ];
  
  let session = Session;
  let mut gen = RandomTable::new(&session, &types, 5);
  let mut mem = MemTable::new(&session, &vec![f32_ty(), i32_ty()], &vec!["x", "y"]);
  
  let page      = gen.next().unwrap();
  let projected = page.project(&vec![1,2]);
  
  assert_eq!(5, page.value_count());
  assert_eq!(5, projected.value_count());
  
  assert_eq!(3, page.minipage_num());
  assert_eq!(2, projected.minipage_num());
  
  mem.write(&projected).ok().unwrap();
  assert_eq!(5, mem.row_num());
  
  let reader = mem.reader();
  let mut row_id = 0usize;
  for x in reader {
  	let r: (f32, i32) = x.ok().unwrap();
  	assert_eq!(page.minipage(1).read_f32(row_id), r.0);
  	assert_eq!(page.minipage(2).read_i32(row_id), r.1);
  	
  	row_id += 1;
  }
}