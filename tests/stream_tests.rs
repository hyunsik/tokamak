extern crate tajo;

use std::str;
use tajo::io::stream::*;

#[bench]
pub fn test_file_read() {
  let mut fin = FileInputStream::new("/home/hyunsik/tpch/lineitem/lineitem.tbl");
  let r = fin.open();
  assert!(r.is_ok());

  let read = fin.read();
  assert!(read.is_ok());

  let buf = read.ok().unwrap();
  println!("{}", buf.len());  
  let s = str::from_utf8(buf.as_slice()).ok().unwrap();
  println!("{}", s);
}