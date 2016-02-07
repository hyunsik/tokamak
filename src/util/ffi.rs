#[macro_export]
macro_rules! from_cstr {
  ( $cstr:expr) => {
	  unsafe {
	  	let c_str = ::std::ffi::CStr::from_ptr($cstr);
      let bytes = c_str.to_bytes();
      ::std::str::from_utf8(bytes).unwrap()
	  }
	}
}
