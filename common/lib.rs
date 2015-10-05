#![feature(alloc)]
#![feature(const_fn)]
#![feature(heap_api)]
#![feature(libc)]
#![feature(raw)]
extern crate alloc;
extern crate itertools;
extern crate libc;

pub mod err;
pub mod func;
pub mod mm;
pub mod rows;
pub mod schema;
pub mod str;
pub mod types;
pub mod platform;