use io::{Storage, BlockStorage};

pub struct LocalFs {
  path: String
}

impl LocalFs {
  fn new() {

  }
}

impl Storage for LocalFs {
  fn uri(&self) -> String {
   self.path.clone()
 }

 fn available_capacity(&self) -> u64 {
   0
 }

 fn total_capacity(&self) -> u64 {
   0
 }
}

impl BlockStorage for LocalFs {
  fn split(&self, url: String, size : u64) {}

  fn list_files(url : String) {}
}