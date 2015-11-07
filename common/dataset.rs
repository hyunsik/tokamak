use std::rc::Rc;

use uuid::Uuid;

use memtable::MemTable;
use types::Ty;

#[derive(Clone)]
pub struct DataSet<'a> {
  id: String,
  pub decl: DataSetDecl<'a>
}

#[derive(Clone)]
pub enum DataSetDecl<'a> {
  // types and rownum
  RandomTable(Vec<Ty>, usize),
  MemoryTable(Rc<MemTable<'a>>)
}

impl<'a> DataSet<'a> {
  pub fn random(types: Vec<Ty>, rownum: usize) -> DataSet<'a> {
    DataSet {
      id: Uuid::new_v4().to_hyphenated_string(),
      decl: DataSetDecl::RandomTable(types, rownum)
    }
  }
}