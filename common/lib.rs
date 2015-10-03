#![feature(alloc)]
#![feature(const_fn)]
#![feature(heap_api)]
#![feature(libc)]
#![feature(raw)]
extern crate alloc;
extern crate itertools;
extern crate libc;

pub mod err;
pub mod str;
pub mod schema;
pub mod mm;
pub mod types;
pub mod platform;
pub mod rows;