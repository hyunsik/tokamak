use std::mem;
use std::marker;

#[derive(Debug, Clone, Copy)]
pub struct Buf<'b> {
  ptr: usize,
  limit: usize,
  _marker: marker::PhantomData<&'b ()>,
}

impl<'b> Buf<'b> {
  pub fn new(ptr: usize, limit: usize) -> Buf<'b> {
    Buf {
      ptr: ptr,
      limit: limit,
      _marker: marker::PhantomData,
    }
  }

  pub fn remain(&self) -> usize {
    self.limit - self.ptr
  }
}

pub trait UnSafeDatumWriter<'b> {
  fn write_bool(&mut self, value: bool);
  fn write_i8(&mut self, value: i8);
  fn write_i16(&mut self, value: i16);
  fn write_i32(&mut self, value: i32);
  fn write_i64(&mut self, value: i64);
  fn write_f32(&mut self, value: f32);
  fn write_f64(&mut self, value: f64);
}

pub trait UnSafeDatumReader<'b> {
  fn read_bool(&mut self) -> bool;
  fn read_i8(&mut self) -> i8;
  fn read_i16(&mut self) -> i16;
  fn read_i32(&mut self) -> i32;
  fn read_i64(&mut self) -> i64;
  fn read_f32(&mut self) -> f32;
  fn read_f64(&mut self) -> f64;
}

impl<'b> UnSafeDatumWriter<'b> for Buf<'b> {
  fn write_bool(&mut self, value: bool) {
    debug_assert!((self.ptr + mem::size_of::<i8>()) <= self.limit,
                  "buffer overrun");

    unsafe {
      *(self.ptr as *mut i8) = value as i8;
    }

    self.ptr += mem::size_of::<i8>();
  }
  fn write_i8(&mut self, value: i8) {
    debug_assert!((self.ptr + mem::size_of::<i8>()) <= self.limit,
                  "buffer overrun");

    unsafe {
      *(self.ptr as *mut i8) = value;
    }

    self.ptr += mem::size_of::<i8>();
  }

  fn write_i16(&mut self, value: i16) {
    debug_assert!((self.ptr + mem::size_of::<i16>()) <= self.limit,
                  "buffer overrun");

    unsafe {
      *(self.ptr as *mut i16) = value;
    }

    self.ptr += mem::size_of::<i16>();
  }

  fn write_i32(&mut self, value: i32) {
    debug_assert!((self.ptr + mem::size_of::<i32>()) <= self.limit,
                  "buffer overrun");

    unsafe {
      *(self.ptr as *mut i32) = value;
    }

    self.ptr += mem::size_of::<i32>();
  }

  fn write_i64(&mut self, value: i64) {
    debug_assert!((self.ptr + mem::size_of::<i64>()) <= self.limit,
                  "buffer overrun");

    unsafe {
      *(self.ptr as *mut i64) = value as i64;
    }

    self.ptr += mem::size_of::<i64>();
  }

  fn write_f32(&mut self, value: f32) {
    debug_assert!((self.ptr + mem::size_of::<f32>()) <= self.limit,
                  "buffer overrun");

    unsafe {
      *(self.ptr as *mut f32) = value;
    }

    self.ptr += mem::size_of::<i32>();
  }

  fn write_f64(&mut self, value: f64) {
    debug_assert!((self.ptr + mem::size_of::<f64>()) <= self.limit,
                  "buffer overrun");

    unsafe {
      *(self.ptr as *mut f64) = value;
    }

    self.ptr += mem::size_of::<f64>();
  }
}

impl<'b> UnSafeDatumReader<'b> for Buf<'b> {
  fn read_bool(&mut self) -> bool {
    debug_assert!((self.ptr + mem::size_of::<i8>()) <= self.limit,
                  "buffer overrun");

    let ptr = self.ptr;
    self.ptr += mem::size_of::<i8>();

    unsafe { *(ptr as *const bool) }
  }

  fn read_i8(&mut self) -> i8 {
    debug_assert!((self.ptr + mem::size_of::<i8>()) <= self.limit,
                  "buffer overrun");

    let ptr = self.ptr;
    self.ptr += mem::size_of::<i8>();

    unsafe { *(ptr as *const i8) }
  }
  fn read_i16(&mut self) -> i16 {
    debug_assert!((self.ptr + mem::size_of::<i8>()) <= self.limit,
                  "buffer overrun");

    let ptr = self.ptr;
    self.ptr += mem::size_of::<i16>();

    unsafe { *(ptr as *const i16) }
  }
  fn read_i32(&mut self) -> i32 {
    debug_assert!((self.ptr + mem::size_of::<i8>()) <= self.limit,
                  "buffer overrun");

    let ptr = self.ptr;
    self.ptr += mem::size_of::<i32>();

    unsafe { *(ptr as *const i32) }
  }
  fn read_i64(&mut self) -> i64 {
    debug_assert!((self.ptr + mem::size_of::<i8>()) <= self.limit,
                  "buffer overrun");

    let ptr = self.ptr;
    self.ptr += mem::size_of::<i64>();

    unsafe { *(ptr as *const i64) }
  }
  fn read_f32(&mut self) -> f32 {
    debug_assert!((self.ptr + mem::size_of::<i8>()) <= self.limit,
                  "buffer overrun");

    let ptr = self.ptr;
    self.ptr += mem::size_of::<f32>();

    unsafe { *(ptr as *const f32) }
  }
  fn read_f64(&mut self) -> f64 {
    debug_assert!((self.ptr + mem::size_of::<i8>()) <= self.limit,
                  "buffer overrun");

    let ptr = self.ptr;
    self.ptr += mem::size_of::<f64>();

    unsafe { *(ptr as *const f64) }
  }
}
