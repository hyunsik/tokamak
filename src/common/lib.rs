#![feature(alloc)]
#![feature(const_fn)]
#![feature(heap_api)]
#![feature(libc)]
#![feature(raw)]
extern crate alloc;
extern crate itertools;
extern crate libc;
extern crate rand;
extern crate rustc_serialize;
extern crate uuid;

extern crate util;

//pub mod dataset;
pub mod err;
pub mod func;
//pub mod input;
pub mod mm;
pub mod page;
pub mod plugin;
pub mod session;
pub mod str;
pub mod types;
pub mod platform;

//mod memtable;
//mod random_table;
/*
pub mod storage {
	pub use memtable::*;
	pub use random_table::*;
}*/