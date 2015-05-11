extern crate tajo;

use std::str;
use tajo::io::stream::*;

#[test]
pub fn test_file_read() {

  let mut fin = FileInputStream::new("/home/hyunsik/tpch/lineitem/lineitem.tbl");
  let mut reader: Reader<FileInputStream> = Reader::new(fin);
  
  let r = reader.open();
  assert!(r.is_ok());

  let read = reader.read();
  assert!(read.is_ok());

  let buf = read.ok().unwrap();
  println!("{}", buf.len());  
  let s = str::from_utf8(buf.as_slice()).ok().unwrap();
  println!("{}", s);
}