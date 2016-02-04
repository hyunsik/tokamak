use std::fmt::Write;

pub extern "C" fn encode_i8(buf: &mut String, val: i8) {
  write!(buf, "{}", val).unwrap();
}

pub extern "C" fn encode_i8(buf: &mut String, val: i8) {
  write!(buf, "{}", val).unwrap();
}