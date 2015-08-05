use libc::{c_char, c_int, c_ulong};
use std::ffi::CString;
use std::mem;

#[repr(C)]
#[derive(Clone, Copy)]
pub struct statvfs {
  pub f_bsize:   c_ulong,
  pub f_frsize:  c_ulong,
  pub f_blocks:  c_ulong, // fsblkcnt_t
  pub f_bfree:   c_ulong, // fsblkcnt_t
  pub f_bavail:  c_ulong, // fsblkcnt_t
  pub f_files:   c_ulong, // fsfilcnt_t
  pub f_ffree:   c_ulong, // fsfilcnt_t
  pub f_favail:  c_ulong, // fsfilcnt_t
  pub f_fsid:    c_ulong,
  pub f_flag:    c_ulong,
  pub f_namemax: c_ulong
}

extern "C" {
  pub fn statvfs(path: *const c_char, buf: *mut statvfs) -> c_int;
  pub fn fstatvfs(fd: c_int, buf: *mut statvfs) -> c_int;
}

#[test]
fn test_statvfs() {
	
  let path = CString::new("/etc/passwd");
  unsafe {
    let mut buf = mem::zeroed();
    let ret = statvfs(path.unwrap().as_ptr(), &mut buf);    
    assert_eq!(ret, 0);
  }
}