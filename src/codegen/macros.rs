macro_rules! llvm_ret(
  ($ret:expr, $out:expr, $err:expr) => (
    if $ret == 0 {
      Ok($out)
    } else {
      Err(chars_to_str($err).to_string())
    }
  );
);

macro_rules! expect_noerr(
  ($ret:expr, $message:expr) => (
    if $ret == 1 {
      return Err($message.to_string());
    }
  );  
);

macro_rules! impl_dispose (
  ($ty:ty, $func:expr) => (
    impl Drop for $ty {
      fn drop(&mut self) {
        unsafe {
          $func(self.0);
        }
      }
    }    
  );
);

macro_rules! impl_display(
	($ty:ty, $func:ident) => (
  	impl fmt::Debug for $ty {
    	fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
				fmt.write_str(unsafe {
        	let c_str = core::$func(self.0);
        	::util::chars_to_str(c_str)
        })
		  }
    }
  	
  	impl fmt::Display for $ty {
    	fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
				fmt.write_str(unsafe {
        	let c_str = core::$func(self.0);
        	::util::chars_to_str(c_str)
        })
		  }
    }
  );
);