use std::io;

// TODO : should contain more detailed information
#[allow(non_camel_case_types)]
#[derive(Clone, Debug)]
pub enum Error {
  InternalError,
  NotImplemented,
  UnsupportedFeature,
  InvalidRpcCall,
  
  UndefinedTablespace,
  UndefinedDatabase,
  UndefinedTable,
  UndefinedColumn,
  UndefinedOperator,
  UndefinedDataType(String),

  /// No tablespace handler for a given URL
  UnsupportedTableSpace,

  NoLineDelimiter,

  // Table Space
  /// Unsupported format in a specify tablespace
  UnsupportedTableFormat,

  /// Invoked function is not implemented yet
  Unimplemented,
  InvalidExpression,
  
  /// Duplicated Type Id
  DuplicatedFuncSign,
  DuplicatedTypeId,
}

pub type Result<T> = ::std::result::Result<T, Error>;

pub type Void = Result<()>;

pub const void_ok : Void = Ok(());

impl From<io::Error> for Error {
  fn from(e: io::Error) -> Error {
    match e {
      _ => Error::InternalError
    }
  }
}