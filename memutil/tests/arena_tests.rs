#![feature(core)]
extern crate bytesize;
extern crate memutil;
use std::raw::Slice;

use bytesize::ByteSize;
use memutil::{Arena, UnSafeDatumWriter, UnSafeDatumReader};

use std::mem;

#[test]
fn test_buf_read_write() {
  let mut arena = Arena::new(ByteSize::kb(16).as_usize());
  let mut buf = arena.alloc(ByteSize::kb(1).as_usize());
  let mut copy = buf;

  buf.write_bool(true);
  buf.write_i8(1);
  buf.write_i16(9);
  buf.write_i32(8);
  buf.write_i64(0);
  buf.write_f32(4.0);
  buf.write_f64(1.0);

  assert_eq!(copy.read_bool(), true);
  assert_eq!(copy.read_i8(), 1);
  assert_eq!(copy.read_i16(), 9);
  assert_eq!(copy.read_i32(), 8);
  assert_eq!(copy.read_i64(), 0);
  assert_eq!(copy.read_f32(), 4 as f32);
  assert_eq!(copy.read_f64(), 1 as f64);
}

#[test] 
#[should_panic(expected = "buffer overrun")]
fn test_write_buf_overrun() {
  let mut arena = Arena::new(ByteSize::kb(16).as_usize());
  let mut buf = arena.alloc(ByteSize::b(16).as_usize());

  buf.write_i32(1);
  buf.write_i32(2);
  buf.write_i32(3);
  buf.write_i32(4);
  buf.write_i32(4); // this write exceeds allocated buffer size
}

#[test]
fn test_vec_and_slice() {
  let xy: Vec<u8> = vec![0,1,2,3];
  let slice = xy.as_ptr() as *const u8;
}

#[test]
fn test_str() {
  let str = "text".to_string();
  assert_eq!(str.len(), 4);

  let str2 = "text";
  let bytes = str2.as_ptr() as *const u8;

  let x = Slice {data: bytes, len: str2.len()};
}