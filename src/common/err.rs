use std::io;

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Debug)]
pub enum Error {
  Unknown,

  ColumnNotFound,
  TableNotFound,
  NoDatabasde,

  NoLineDelimiter,
}

pub type TResult<T> = Result<T, Error>;

pub type Void = Result<(), Error>;

#[inline]
pub fn void_ok() -> Void {
  Ok(())
}

impl From<io::Error> for Error {
  fn from(e: io::Error) -> Error {
    match e {
      _ => Error::Unknown
    }
  }
}