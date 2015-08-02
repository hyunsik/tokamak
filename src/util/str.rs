//! A slice for bytes array representing a string
//!
//! StrSlice is designed for zero-copy string operations. 
//! It should be carefully used because it just contains borrowed contents.
//! StrSlice looks similar to rust's `str`, but it just handles a sequence of 
//! bytes instead of unicode. In other words, it does not consider any encoding.
//! Also, it internally uses highly low-level optimization techniques like SSE 4.2.
//!
//! # Examples
//!
//! ```ignore
//! let str1 = StrSlice::new_from_str("aaaaa");
//! let str2 = StrSlice::new_from_str("aaaaa");
//! let cmp = str1 >= str2
//! ...
//! ```

use libc::funcs::c95::string::memcmp;
use libc::types::common::c95::c_void;
use std::cmp;
use std::cmp::Ordering;
use std::fmt::{Error, Display, Formatter};
use std::mem;
use std::raw::Slice;
use std::result::Result;
use std::str;



#[derive(Copy, Clone, Debug)]
#[repr(C)]
#[allow(raw_pointer_derive)]
pub struct StrSlice {
  ptr: *const u8,
  len: i32,
}

impl StrSlice {  
  #[inline]
  pub fn new(ptr: *const u8, len: i32) -> StrSlice {
    StrSlice {
      ptr: ptr,
      len: len
    }
  }
  #[inline]
  pub fn new_from_str<'a>(s: &str) -> StrSlice {
    StrSlice {
      ptr: s.as_ptr(),
      len: s.len() as i32
    }
  }

  #[inline]
  pub fn set_ptr(&mut self, ptr: *const u8) {
    self.ptr = ptr;
  }

  #[inline]
  pub fn set_len(&mut self, len: i32) {
    self.len = len;
  }

  #[inline]
  pub fn as_ptr(&self) -> *const u8 {
    self.ptr
  }

  #[inline]
  pub fn len(&self) -> i32 {
    self.len
  }

  #[inline]
  pub fn to_slice<'a>(&'a self) -> &'a [u8] {
    let slice = Slice {data: self.ptr, len: (self.len as usize)};
    unsafe {
      mem::transmute(slice)
    }
  }

  #[inline]
  pub fn to_str<'a>(&'a self) -> &'a str {
    str::from_utf8(self.to_slice()).unwrap()
  }

  #[inline]
  pub fn to_string(&self) -> String {
    String::from_str(self.to_str())
  }
}

impl Display for StrSlice {
  fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
    Display::fmt(self.to_str(), f)
  }
}

impl PartialEq for StrSlice {
  #[inline]
  fn eq(&self, other: &StrSlice) -> bool {
    self.len() == other.len() && rawcmp(self.ptr, other.ptr, self.len) == 0
  }

  #[inline]
  fn ne(&self, other: &StrSlice) -> bool {
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

impl PartialOrd for StrSlice {
    #[inline]
    fn partial_cmp(&self, other: &StrSlice) -> Option<Ordering> {
      None
    }

    #[inline]
    fn lt(&self, other: &StrSlice) -> bool {      
      let cmp = rawcmp(self.ptr, other.ptr, cmp::min(self.len, other.len));
      match cmp {
        0 => (self.len() - other.len()) < 0,
        _ => cmp < 0
      }
    }

    #[inline]
    fn le(&self, other: &StrSlice) -> bool {
      let cmp = rawcmp(self.ptr, other.ptr, cmp::min(self.len, other.len));
      match cmp {
        0 => (self.len() - other.len()) <= 0,
        _ => cmp <= 0
      }
    }

    #[inline]
    fn gt(&self, other: &StrSlice) -> bool { 
      let cmp = rawcmp(self.ptr, other.ptr, cmp::min(self.len, other.len));
      match cmp {
        0 => (self.len() - other.len()) > 0,
        _ => cmp > 0
      }
    }

    #[inline]
    fn ge(&self, other: &StrSlice) -> bool { 
      let cmp = rawcmp(self.ptr, other.ptr, cmp::min(self.len, other.len));
      match cmp {
        0 => (self.len() - other.len()) >= 0,
        _ => cmp >= 0
      }
    }
}