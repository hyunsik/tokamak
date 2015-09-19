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

use alloc::heap;
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
  pub fn from_str<'a>(s: &str) -> StrSlice {
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
  pub fn as_slice<'a>(&'a self) -> &'a [u8] {
    let slice = Slice {data: self.ptr, len: (self.len as usize)};
    unsafe {
      mem::transmute(slice)
    }
  }

  #[inline]
  pub fn as_slice_mut<'a>(&'a self) -> &'a mut [u8] {
    let slice = Slice {data: self.ptr, len: (self.len as usize)};
    unsafe {
      mem::transmute(slice)
    }
  }

  #[inline]
  pub fn as_str<'a>(&'a self) -> &'a str {
    str::from_utf8(self.as_slice()).unwrap()
  }

  #[inline]
  pub fn to_string(&self) -> String {
    String::from(self.as_str())
  }
}

pub unsafe fn alloc_str_slice(len: usize, align: usize) -> StrSlice {
  StrSlice {
    ptr: heap::allocate(len, align),
    len: len as i32
  }
}

/// Deallocate the pointer contained in the StrSlice with the align size
/// After deallocation, do not use this StrSlice
pub unsafe fn dealloc_str_slice(slice: &mut StrSlice, align: usize) {
  heap::deallocate(slice.as_ptr() as *mut u8, slice.len() as usize, align);
  slice.set_ptr(0 as *mut u8);
  slice.set_len(0 as i32);
}

impl Display for StrSlice {
  fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
    Display::fmt(self.as_str(), f)
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

/// Split a slice with the delimiter into multiple slices.
/// Return tuple contains the length involved in the output slices
/// and how many slices are filled in out_slices.
#[inline]
pub unsafe fn split_str_slice(slice: &mut StrSlice,
                   out_slices: &mut [StrSlice],
                   delim: u8) -> (usize, usize) {  
  let mut split_idx  : usize = 0;
  let mut last_pos   : usize = 0; // keep the start offset
  let mut cur_pos    : usize = 0; // the current offset

  let line_len = slice.len() as usize;

  while cur_pos < line_len && split_idx < out_slices.len() {
    let c: u8 = *slice.as_ptr().offset(cur_pos as isize);

    // check two cases:
    // * if the character is line delimiter
    // * if this position is the last position in this line
    if c == delim || cur_pos == (line_len - 1) {
      // set the str slice
      out_slices[split_idx].set_ptr(slice.as_ptr().offset(last_pos as isize));
      out_slices[split_idx].set_len((cur_pos - last_pos) as i32);

      // move forward the indices
      last_pos = cur_pos + 1;
      split_idx = split_idx + 1;
    }

    cur_pos = cur_pos + 1;
  }

  // 1) the splitted length 2) how many splits are found?
  (last_pos, split_idx)
}

#[test]
fn test_split_str_slice_case1() {
  // less than num of output slices
  let mut input = StrSlice::from_str("aaa,bbb,ccc,ddd,eee");
  let mut slices: [StrSlice;6] = unsafe { mem::zeroed() };
  let res = unsafe {split_str_slice(&mut input, &mut slices, ',' as u8)};
  assert_eq!(input.len() as usize, res.0);
  assert_eq!(5, res.1);
}

#[test]
fn test_split_str_slice_case2() {
  // equal to num of output slices
  let mut input = StrSlice::from_str("aaa,bbb,ccc,ddd,eee,fff");
  let mut slices: [StrSlice;6] = unsafe { mem::zeroed() };
  let res = unsafe {split_str_slice(&mut input, &mut slices, ',' as u8)};
  assert_eq!(input.len() as usize, res.0);
  assert_eq!(6, res.1);
}

#[test]
fn test_split_str_slice_case3() {
  // greater than num of output slices
  let mut input = StrSlice::from_str("aaa,bbb,ccc,ddd,eee,fff,ggg,hhh");
                                   // 0123456789012345678901234567890
  let mut slices: [StrSlice;6] = unsafe { mem::zeroed() };
  let res = unsafe {split_str_slice(&mut input, &mut slices, ',' as u8)};
  assert_eq!(24 as usize, res.0);
  assert_eq!(6, res.1);
}
