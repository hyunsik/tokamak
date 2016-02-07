use std::rc::Rc;

use uuid::Uuid;

use memtable::MemTable;
use types::Ty;

#[derive(Clone)]
pub struct DataSet {
  id: String,
  pub decl: DataSetDecl,
}

#[derive(Clone)]
pub enum DataSetDecl {
  // types and rownum
  RandomTable(Vec<Ty>, usize),
  MemoryTable(Rc<MemTable>),
}

impl DataSet {
  pub fn random(types: Vec<Ty>, rownum: usize) -> DataSet {
    DataSet {
      id: Uuid::new_v4().to_hyphenated_string(),
      decl: DataSetDecl::RandomTable(types, rownum),
    }
  }
}
