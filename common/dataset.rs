use uuid::Uuid;

use types::Ty;

pub struct DataSet {
  id: String,
  decl: DataSetDecl
}

pub enum DataSetDecl {
  // types and rownum
  RandomTable(Vec<Ty>, usize)
}

impl DataSet {
  pub fn random(types: Vec<Ty>, rownum: usize) -> DataSet {
    DataSet {
      id: Uuid::new_v4().to_hyphenated_string(),
      decl: DataSetDecl::RandomTable(types, rownum)
    }
  }
}