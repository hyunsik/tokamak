use std::any::Any;

#[cfg(windows)]
#[allow(bad_style)]
pub fn acquire_global_lock(name: &str) -> Box<Any> {
  use std::ffi::CString;
  use std::io;

  type LPSECURITY_ATTRIBUTES = *mut u8;
  type BOOL = i32;
  type LPCSTR = *const u8;
  type HANDLE = *mut u8;
  type DWORD = u32;

  const INFINITE: DWORD = !0;
  const WAIT_OBJECT_0: DWORD = 0;
  const WAIT_ABANDONED: DWORD = 0x00000080;

  extern "system" {
    fn CreateMutexA(lpMutexAttributes: LPSECURITY_ATTRIBUTES,
                    bInitialOwner: BOOL,
                    lpName: LPCSTR) -> HANDLE;
    fn WaitForSingleObject(hHandle: HANDLE,
                           dwMilliseconds: DWORD) -> DWORD;
    fn ReleaseMutex(hMutex: HANDLE) -> BOOL;
    fn CloseHandle(hObject: HANDLE) -> BOOL;
  }

  struct Handle(HANDLE);

  impl Drop for Handle {
    fn drop(&mut self) {
      unsafe {
        CloseHandle(self.0);
      }
    }
  }

  struct Guard(Handle);

  impl Drop for Guard {
    fn drop(&mut self) {
      unsafe {
        ReleaseMutex((self.0).0);
      }
    }
  }

  let cname = CString::new(name).unwrap();
  unsafe {
    // Create a named mutex, with no security attributes and also not
    // acquired when we create it.
    //
    // This will silently create one if it doesn't already exist, or it'll
    // open up a handle to one if it already exists.
    let mutex = CreateMutexA(0 as *mut _, 0, cname.as_ptr() as *const u8);
    if mutex.is_null() {
      panic!("failed to create global mutex named `{}`: {}", name,
                   io::Error::last_os_error());
    }
    let mutex = Handle(mutex);

    // Acquire the lock through `WaitForSingleObject`.
    //
    // A return value of `WAIT_OBJECT_0` means we successfully acquired it.
    //
    // A return value of `WAIT_ABANDONED` means that the previous holder of
    // the thread exited without calling `ReleaseMutex`. This can happen,
    // for example, when the compiler crashes or is interrupted via ctrl-c
    // or the like. In this case, however, we are still transferred
    // ownership of the lock so we continue.
    //
    // If an error happens.. well... that's surprising!
    match WaitForSingleObject(mutex.0, INFINITE) {
      WAIT_OBJECT_0 | WAIT_ABANDONED => {}
      code => {
        panic!("WaitForSingleObject failed on global mutex named \
                        `{}`: {} (ret={:x})", name,
                       io::Error::last_os_error(), code);
      }
    }

    // Return a guard which will call `ReleaseMutex` when dropped.
    Box::new(Guard(mutex))
  }
}

#[cfg(unix)]
pub fn acquire_global_lock(_name: &str) -> Box<Any> {
  Box::new(())
}
