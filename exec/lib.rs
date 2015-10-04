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
extern crate itertools;
extern crate libc;

extern crate common;
extern crate sql;
extern crate storage;

pub mod types;
pub mod scan;

use std::rc::Rc;

use common::err::{Void, TResult, Error};
use common::rows::{Page, PageBuilder};
use storage::InputSource;

pub trait Executor {
  fn init      (&mut self) -> Void;
  fn need_input(&self) -> bool;
  fn add_input (&mut self, &Page) -> Void;
  fn get_output(&mut self) -> TResult<&Page>;
  fn close     (&mut self) -> Void;
}

pub trait OperatorFactory {
  fn create(&self) -> Box<Executor>;
}

pub trait Processor {
  fn process(page: &Page, page_builder: &PageBuilder) -> Void;
}

pub trait InputSourceProvider {
  fn create(&self) -> Box<InputSource>;
}

/*
#[test]
pub fn test_pipeline() {
  let page_builder: Box<PageBuilder> = Box::new(DefaultPageBuilder);
  
  let input_page = page_builder.create(&vec![
    Box::new(Int4Type),
    Box::new(Float4Type),
  ]);
  
  let mut output_page = page_builder.create(&vec![
    Box::new(Int4Type),
    Box::new(Float4Type),
  ]);
}
*/