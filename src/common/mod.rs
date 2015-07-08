pub use self::schema::{Column, Schema};
pub use self::err::{Error, TResult, Void};
pub use self::string_slice::StringSlice;

pub mod constant;
pub mod types;
pub mod schema;
pub mod err;
pub mod string_slice;

pub struct P<T> {
  ptr: Box<T>
}