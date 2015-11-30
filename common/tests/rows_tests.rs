#[macro_use] extern crate common;

use common::session::Session;
use common::types::{F32, I32};
use common::rows::{Page, MiniPage, ROWBATCH_SIZE};
use common::input::InputSource;
use common::storage::RandomTable;

#[test]
pub fn test_minipage_copy() 
{
  let schema = schema!(I32, F32);
  
  let session = Session;
  let mut generator = RandomTable::new(&session, &schema, ROWBATCH_SIZE);
  
  {
		let page = generator.next().unwrap();
		let m1: &MiniPage = page.minipage(0);
		let m2: &MiniPage = page.minipage(1);	  
	  
	  let m1_copy = m1.copy();
	  let m2_copy = m2.copy();
	  
	  for x in 0 .. ROWBATCH_SIZE {
	    assert_eq!(m1.read_i32(x), m1_copy.read_i32(x));
	    assert_eq!(m2.read_i32(x), m2_copy.read_i32(x));
	  }
  }
  
  generator.close().ok().unwrap();
}

#[test]
pub fn test_project() 
{
  let schema = schema!(I32, F32, F32);
  
  let session = Session;
  let mut gen = RandomTable::new(&session, &schema, 5);
  
  let page      = gen.next().unwrap();
  let projected = page.project(&vec![1,2]);
  
  assert_eq!(5, page.value_count());
  assert_eq!(5, projected.value_count());
  
  assert_eq!(3, page.minipage_num());
  assert_eq!(2, projected.minipage_num());
  
  for x in 0 .. 5 {
		assert_eq!(page.minipage(1).read_f32(x), projected.minipage(0).read_f32(x));
		assert_eq!(page.minipage(2).read_i32(x), projected.minipage(1).read_i32(x));
  }
}

#[test]
pub fn test_page_copy() 
{
  let schema = schema!(I32, F32);
  
  let session = Session;
  let mut generator = RandomTable::new(&session, &schema, ROWBATCH_SIZE);
  
  {
		let page = generator.next().unwrap();
		let copied_page = page.to_owned();
		
		let m1 = page.minipage(0);
		let m2 = page.minipage(1);	  
	  
	  let m1_copy = copied_page.minipage(0);
	  let m2_copy = copied_page.minipage(1);
	  
	  for x in 0 .. ROWBATCH_SIZE {
	    assert_eq!(m1.read_i32(x), m1_copy.read_i32(x));
	    assert_eq!(m2.read_i32(x), m2_copy.read_i32(x));
	  }
  }
  
  generator.close().ok().unwrap();
}