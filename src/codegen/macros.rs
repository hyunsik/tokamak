macro_rules! llvm_ret(
  ($ret:expr, $out:expr, $err:expr) => (
    if $ret == 0 {
      Ok($out)
    } else {
      Err(::util::chars::to_str($err).to_string())
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

macro_rules! impl_from_ref(
  ($from:ty, $to:ident) => (
    impl From<$from> for $to {
      fn from(r: $from) -> Self {
        $to(r)
      }
    }
  );
);

macro_rules! impl_has_context(
  ($ty:ty, $func:ident) => (
    impl HasContext for $ty {
      fn context(&self) -> LLVMContextRef {
        unsafe { core::$func(self.0) }
      }
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
        	::util::chars::to_str(c_str)
        })
		  }
    }
  	
  	impl fmt::Display for $ty {
    	fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
				fmt.write_str(unsafe {
        	let c_str = core::$func(self.0);
        	::util::chars::to_str(c_str)
        })
		  }
    }
  );
);

macro_rules! to_llvmref_array(
  ($e:expr, $t:ty) => (
    $e.iter().map(|x| x.0).collect::<Vec<$t>>()
  );
);
  