//! A slice for bytes array representing a string
//!
//! StringSlice is designed for zero-copy string operations. 
//! It should be carefully used because it just contains borrowed contents.
//! StringSlice looks similar to rust's `str`, but it just handles a sequence of 
//! bytes instead of unicode. In other words, it does not consider any encoding.
//! Also, it internally uses highly low-level optimization techniques like SSE 4.2.
//!
//! # Examples
//!
//! ```ignore
//! let str1 = StringSlice::new_from_str("aaaaa");
//! let str2 = StringSlice::new_from_str("aaaaa");
//! let cmp = str1 >= str2
//! ...
//! ```

use libc::funcs::c95::string::memcmp;
use libc::types::common::c95::c_void;
use std::mem;
use std::cmp;
use std::cmp::Ordering;
use std::str;
use std::raw::Slice;


#[derive(Debug)]
#[repr(C)]
#[allow(raw_pointer_derive)]
pub struct StringSlice {
  ptr: *const u8,
  len: i32,
}

impl StringSlice {
  

  pub fn new(ptr: *const u8, len: i32) -> StringSlice {
    StringSlice {
      ptr: ptr,
      len: len
    }
  }

  pub fn new_from_str<'a>(s: &str) -> StringSlice {
    StringSlice {
      ptr: s.as_ptr(),
      len: s.len() as i32
    }
  }

  pub fn set_ptr(&mut self, ptr: *const u8) {
    self.ptr = ptr;
  }

  pub fn set_len(&mut self, len: i32) {
    self.len = len;
  }

  pub fn as_ptr(&self) -> *const u8 {
    self.ptr
  }

  pub fn len(&self) -> i32 {
    self.len
  }

  pub fn to_slice<'a>(&'a self) -> &'a [u8] {
    let slice = Slice {data: self.ptr, len: (self.len as usize)};
    unsafe {
      mem::transmute(slice)
    }
  }

  pub fn to_str<'a>(&'a self) -> &'a str {
    str::from_utf8(self.to_slice()).unwrap()
  }

  pub fn to_string(&self) -> String {
    String::from_str(self.to_str())
  }
}

impl PartialEq for StringSlice {
  #[inline]
  fn eq(&self, other: &StringSlice) -> bool {
    self.len() == other.len() && rawcmp(self.ptr, other.ptr, self.len) == 0
  }

  #[inline]
  fn ne(&self, other: &StringSlice) -> bool {
    !(self.eq(other))
  }
}

#[inline]
fn rawcmp(x: *const u8, y: *const u8, len: i32) -> i32 {
  unsafe {
    memcmp(
      x as *const c_void, 
      y as *const c_void, 
      len as u64
    )
  }
}

impl PartialOrd for StringSlice {
    #[inline]
    fn partial_cmp(&self, other: &StringSlice) -> Option<Ordering> {
      None
    }

    #[inline]
    fn lt(&self, other: &StringSlice) -> bool {      
      let cmp = rawcmp(self.ptr, other.ptr, cmp::min(self.len, other.len));
      match cmp {
        0 => (self.len() - other.len()) < 0,
        _ => cmp < 0
      }
    }

    #[inline]
    fn le(&self, other: &StringSlice) -> bool {
      let cmp = rawcmp(self.ptr, other.ptr, cmp::min(self.len, other.len));
      match cmp {
        0 => (self.len() - other.len()) <= 0,
        _ => cmp <= 0
      }
    }

    #[inline]
    fn gt(&self, other: &StringSlice) -> bool { 
      let cmp = rawcmp(self.ptr, other.ptr, cmp::min(self.len, other.len));
      match cmp {
        0 => (self.len() - other.len()) > 0,
        _ => cmp > 0
      }
    }

    #[inline]
    fn ge(&self, other: &StringSlice) -> bool { 
      let cmp = rawcmp(self.ptr, other.ptr, cmp::min(self.len, other.len));
      match cmp {
        0 => (self.len() - other.len()) >= 0,
        _ => cmp >= 0
      }
    }
}