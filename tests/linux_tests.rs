extern crate tajo;

use std::ffi::CString;
use std::mem;

use tajo::native::linux::statvfs;

#[test]
fn test_statvfs() {

  let path = CString::new("/etc/fstab");
  unsafe {
    let mut buf = mem::zeroed();
    let ret = statvfs(path.unwrap().as_ptr(), &mut buf);    
    assert_eq!(ret, 0);
  }
}