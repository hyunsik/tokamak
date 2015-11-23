#![feature(alloc)]
#![feature(const_fn)]
#![feature(heap_api)]
#![feature(libc)]
#![feature(raw)]

///
/// # An overview of Execution Operators
///
/// ## Our goals
///  * Maximize CPU efficiency
///    * less branch misprediction
///    * less function call overhead
///    * more CPU pipelining   
///  * Maximize memory bandwidth (less memory copy)
///
/// # Execution Model
///
/// ## Columnar and Vectorized Processing
///
/// ## Input and output characteristics of operators
///  * Input source - no input / output page required / reuse read buffer itself if possible.
///  * Filter       - Just bypass the page and only sets the selected rows IDs to the page.
///  * Others       - Both input and output pages are required.
/// 

extern crate alloc;
#[macro_use] extern crate itertools;
extern crate libc;

extern crate common;
extern crate plan;
extern crate storage;

pub mod driver;
pub mod split;
pub mod task;
pub mod planner;
pub mod processor;
pub mod projector;

pub mod filter;
pub mod hash_join;
pub mod scan;

use common::err::{Void, Result};
use common::rows::Page;
use common::types::Ty;

use driver::DriverContext;

pub trait Executor 
{
  fn init      (&mut self) -> Void;
  fn need_input(&self) -> bool;
  fn add_input(&mut self, &Page) -> Void;
  fn next(&mut self) -> Result<&Page>;
  fn close     (&mut self) -> Void;
}

pub trait ExecutorFactory 
{
  fn create(&self, ctx: &DriverContext) -> Option<Box<Executor>>;
  
  fn types(&self) -> &Vec<Ty>;
}

pub type Schema<'a, 'b> = (&'a Vec<Ty>, &'a Vec<&'b str>);