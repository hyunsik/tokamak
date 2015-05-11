use common::Error;

pub struct ReadBuffer {
  buf: *const u8,
  buf_len: i64,

  /// the end of the scan range
  eosr: bool,
  /// error 
  err: Option<Error>,
}

impl ReadBuffer {

  pub fn buffer(&self) -> *const u8 {
    self.buf
  }

  pub fn len(&self) -> i64 {
    self.buf_len
  }

  #[inline]
  pub fn is_ok(&self) -> bool {
    self.err.is_some()
  }

  #[inline]
  pub fn is_err(&self) -> bool {
    self.err.is_none()
  }

  #[inline]
  pub fn error(&self) -> Error {
    self.err.unwrap()
  }
}