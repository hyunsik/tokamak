#![feature(oom)]
#![feature(heap_api)]
#![feature(alloc)]
extern crate alloc;
extern crate bytesize;
#[macro_use]
extern crate log;

pub use self::buffer::{Buf, UnSafeDatumWriter, UnSafeDatumReader};
mod buffer;

pub use self::arena::Arena;
mod arena;
