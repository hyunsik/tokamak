use llvm_sys::prelude::LLVMContextRef;

pub trait HasContext {
  fn context(&self) -> LLVMContextRef;
}

pub mod chars {
  use std::ffi::{CStr, CString};
  use libc::c_char;

  #[inline]
  pub fn from_str(s: &str) -> *const c_char {
    CString::new(s.as_bytes()).unwrap().as_ptr()
  }

  #[inline]
  pub fn to_str<'a>(chars: *const c_char) -> &'a str {
    unsafe {
      let c_str: &CStr = CStr::from_ptr(chars);
      let bytes = c_str.to_bytes();
      ::std::str::from_utf8_unchecked(bytes)
    }
  }
  
  #[allow(dead_code)]
  pub fn to_nullable_str<'a>(chars: *const c_char) -> Option<&'a str> {
    unsafe {
      if chars.is_null() {
        None
      } else {
        let c_str: &CStr = CStr::from_ptr(chars);
        let bytes = c_str.to_bytes();
        Some(::std::str::from_utf8_unchecked(bytes))
      }
    }
  }
}