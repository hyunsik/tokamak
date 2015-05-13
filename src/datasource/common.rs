use common::Schema;

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