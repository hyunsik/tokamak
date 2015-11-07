extern crate common;

use common::session::Session;
use common::types::{i32_ty, f32_ty, Ty};
use common::rows::{ROWBATCH_SIZE};
use common::input::InputSource;
use common::storage::RandomTable;
use common::storage::MemTable;


#[test]
pub fn test_memtable() 
{
  let types: Vec<Ty> = vec![
    i32_ty(), 
    f32_ty()
  ];
  
  let session = Session;
  let mut generator = RandomTable::new(&session, &types, ROWBATCH_SIZE);
  let mut memtable  = MemTable::new(&session, &types, &vec!["x", "y"]);
  
  {
		let page = generator.next().unwrap();
		memtable.write(page).ok().unwrap();
  }
  assert_eq!(ROWBATCH_SIZE, memtable.row_num());
  generator.close().ok().unwrap();
  memtable.close().ok().unwrap();
}