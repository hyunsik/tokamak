extern crate common;

use common::session::Session;
use common::types::{i32_ty, f32_ty, Ty};
use common::rows::{MiniPage, ROWBATCH_SIZE};
use common::input::InputSource;
use common::random_table::RandomTable;

#[test]
pub fn test_minipage_copy() 
{
  let types: Vec<Ty> = vec![
    i32_ty(), 
    f32_ty()
  ];
  
  let session = Session;
  let mut generator = RandomTable::new(&session, &types, ROWBATCH_SIZE);
  
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
pub fn test_page_copy() 
{
  let types: Vec<Ty> = vec![
    i32_ty(), 
    f32_ty()
  ];
  
  let session = Session;
  let mut generator = RandomTable::new(&session, &types, ROWBATCH_SIZE);
  
  {
		let page = generator.next().unwrap();
		
		let copied_page = page.copy();
		
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