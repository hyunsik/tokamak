#[macro_use] extern crate common;

use common::session::Session;
use common::types::{F32, I32, Ty};
use common::page::{c_api, Chunk, ROWBATCH_SIZE};
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
  let session = Session;
  let mut gen = RandomTable::new(&session, &[I32, F32], 5);

  assert_next_rows!(gen, 5);
  assert_next_rows!(gen, 0);
}

#[test]
pub fn test_next_once2()
{
  let session = Session;
  let mut gen = RandomTable::new(&session, &[I32, F32], ROWBATCH_SIZE);

  assert_next_rows!(gen, ROWBATCH_SIZE);
  assert_next_rows!(gen, 0);
}

#[test]
pub fn test_next_multiple()
{
  let session = Session;
  let mut gen = RandomTable::new(&session, &[I32, F32], (ROWBATCH_SIZE * 2) + 100);

  assert_next_rows!(gen, ROWBATCH_SIZE);
  assert_next_rows!(gen, ROWBATCH_SIZE);
  assert_next_rows!(gen, 100);
  assert_next_rows!(gen, 0);
}

#[test]
pub fn test_next_value()
{
  let session = Session;
  let mut gen = RandomTable::new(&session, &[I32, F32], 1024);

  let page = gen.next().unwrap();

  unsafe {
    for x in 0..page.value_count() {
      println!("{} - {},{}", x, c_api::read_raw_i32(page.chunk(0), x), c_api::read_raw_f32(page.chunk(1), x));
    }
  }
}