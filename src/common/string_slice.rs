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
use std::cmp::Eq;
use std::cmp::Ordering;
use std::ops::Add;
use std::str;
use std::raw::Slice;


#[derive(Debug)]
pub struct StringSlice {
  ptr: *const u8,
  len: i32,
}

impl StringSlice {
  
  pub fn as_ptr(&self) -> *const u8 {
    self.ptr
  }

  pub fn len(&self) -> i32 {
    self.len
  }

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

#[test]
fn test_StringSlice_init() {
  let s1 = StringSlice::new_from_str("yate");
  let s2 = StringSlice::new_from_str("가나다");
  let s3 = StringSlice::new_from_str("hyunsik");
  assert_eq!(s1.len(), 4);
  assert_eq!(s2.len(), 9);
  assert_eq!(s3.len(), 7);
}

#[test]
fn test_StringSlice_eq() {
  let s1 = StringSlice::new_from_str("rust");
  let s2 = StringSlice::new_from_str("rust");
  let s3 = StringSlice::new_from_str("hyunsik");

  assert_eq!(s1, s2);
  assert!(!(s2 == s3));
}

#[test]
fn test_StringSlice_ne() {
  let s1 = StringSlice::new_from_str("rust");
  let s2 = StringSlice::new_from_str("yate");
  let s3 = StringSlice::new_from_str("hyunsik");
  let s4 = StringSlice::new_from_str("rust");

  assert!(s1 != s2);
  assert!(s1 != s3);
  assert!(!(s1 != s4));
}

#[test]
fn test_StringSlice_lt() {
  let str1 = StringSlice::new_from_str("aaaaa");
  let str2 = StringSlice::new_from_str("bbbbb");
  assert!(str1 < str2);

  let str3 = StringSlice::new_from_str("aaaaa");
  let str4 = StringSlice::new_from_str("bbbb");
  assert!(str3 < str4);

  let str5 = StringSlice::new_from_str("aaaa");
  let str6 = StringSlice::new_from_str("bbbbb");
  assert!(str5 < str6);

  let str7 = StringSlice::new_from_str("bbbbb");
  let str8 = StringSlice::new_from_str("aaaaa");
  assert!(!(str7 < str8));

  let str9 = StringSlice::new_from_str("bbbb");
  let str10 = StringSlice::new_from_str("aaaaa");
  assert!(!(str9 < str10));

  let str11 = StringSlice::new_from_str("bbbbb");
  let str12 = StringSlice::new_from_str("aaaa");
  assert!(!(str9 < str10));
}

#[test]
fn test_StringSlice_le() {
  let str1 = StringSlice::new_from_str("aaaaa");
  let str2 = StringSlice::new_from_str("aaaaa");
  assert!(str1 <= str2);

  let str3 = StringSlice::new_from_str("aaaaa");
  let str4 = StringSlice::new_from_str("aaaab");
  assert!(str1 <= str2);

  let str5 = StringSlice::new_from_str("aaaa");
  let str6 = StringSlice::new_from_str("aaaab");
  assert!(str5 <= str6);

  let str7 = StringSlice::new_from_str("aaaaa");
  let str8 = StringSlice::new_from_str("aaab");
  assert!(str7 <= str8);
}

#[test]
fn test_StringSlice_gt() {
  let str1 = StringSlice::new_from_str("aaaab");
  let str2 = StringSlice::new_from_str("aaaaa");
  assert!(str1 > str2);

  let str3 = StringSlice::new_from_str("abaaa");
  let str4 = StringSlice::new_from_str("aaaa");
  assert!(str3 > str4);

  let str5 = StringSlice::new_from_str("ab");
  let str6 = StringSlice::new_from_str("aaaa");
  assert!(str5 > str6);

  let str7 = StringSlice::new_from_str("aaaaa");
  let str8 = StringSlice::new_from_str("aaaab");
  assert!(!(str7 > str8));

  let str9 = StringSlice::new_from_str("aaaa");
  let str10 = StringSlice::new_from_str("abaaa");
  assert!(!(str9 > str10));

  let str11 = StringSlice::new_from_str("aaaa");
  let str12 = StringSlice::new_from_str("abaaa");
  assert!(!(str11 > str12));
}

#[test]
fn test_StringSlice_ge() {
  let str1 = StringSlice::new_from_str("aaaaa");
  let str2 = StringSlice::new_from_str("aaaaa");
  assert!(str1 >= str2);

  let str3 = StringSlice::new_from_str("aaaab");
  let str4 = StringSlice::new_from_str("aaaaa");
  assert!(str1 >= str2);

  let str5 = StringSlice::new_from_str("aaaab");
  let str6 = StringSlice::new_from_str("aaaa");
  assert!(str5 >= str6);

  let str7 = StringSlice::new_from_str("aaab");
  let str8 = StringSlice::new_from_str("aaaaa");
  assert!(str7 >= str8);
}