use common::Schema;
use io::Storage;

pub struct DataSource {
  schema: Schema,
  uri: String
}

pub struct BlockStorageSource {
  uri: String
}

pub struct RowStorageSource {
  uri: String
}