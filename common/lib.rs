#![feature(alloc)]
#![feature(const_fn)]
#![feature(heap_api)]
#![feature(libc)]
#![feature(raw)]
extern crate alloc;
extern crate itertools;
extern crate libc;
extern crate uuid;

//pub mod datum;
pub mod err;
pub mod func;
pub mod input;
pub mod mm;
pub mod operator;
pub mod rows;
pub mod plan;
pub mod plugin;
pub mod str;
pub mod types;
pub mod platform;