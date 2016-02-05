use std::fmt::Write;
use std::convert::Into;
use llvm::builder::Builder;
use llvm::value::Value;
use common::types::Ty;

macro_rules! encode_value(
  ($name:ident, $ty:ty) => (
    #[allow(dead_code)]
    pub extern "C" fn $name(buf: &mut String, val: $ty) {
      write!(buf, "{}", val).unwrap();
    }
  );
);

encode_value!(encode_i8, i8);
encode_value!(encode_i16, i16);
encode_value!(encode_i32, i32);
encode_value!(encode_i64, i64);
encode_value!(encode_f32, f32);
encode_value!(encode_f64, f64);

pub fn write<V: Into<Value>>(buf: &mut String, val: &V) {
}