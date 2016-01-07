#![crate_type = "dylib"]

#[repr(i8)]
pub enum MiniPageType {
  RAW = 0,
  RLE = 1,
  BITMAP = 2 
}

#[repr(C)]
pub struct FMiniPage {
  pub ptr  : *mut u8,
  pub size : usize  
}

pub extern "C" fn write_i8(p: *mut FMiniPage, idx: u16, val: i8) {
  
}

#[no_mangle]
pub fn dummy(p1: &MiniPageType, p2: &FMiniPage) {  
}