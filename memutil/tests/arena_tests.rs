extern crate bytesize;
extern crate memutil;

use bytesize::ByteSize;
use memutil::{Arena, UnSafeDatumWriter, UnSafeDatumReader};

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