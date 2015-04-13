use url::Url;
use io::{Storage, BlockStorage};

pub struct LocalFs {
  path: Url
}

impl LocalFs {
  fn new() {

  }
}

impl Storage for LocalFs {
  fn uri(&self) -> Url {
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
  fn split(&self, url: Url, size : u64) {}

  fn list_files(url : Url) {}
}