//! DataFrame is an interface to allow users to easily access a data set.
//! It provides a schema, a data access pointer, and various metadata about 
//! the data set.

use common::Schema;
use datasource::common::DataSource;

pub struct DataSet {
  schema : Schema
}

impl DataSet {
  fn project(field_indices: &[u32]) {
  }
  
  fn filter(condition : String) {
  }
  
  fn groupby(keys: Vec<String>) {
  }
}

fn dataset(uri: String) -> Option<DataSet> {
  None
}

fn dataset_with(uri : String, storage_type : String) -> Option<DataSet> {
   None
}