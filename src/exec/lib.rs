#![feature(alloc)]
#![feature(const_fn)]
#![feature(libc)]

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
#[macro_use]
extern crate itertools;
extern crate libc;
#[macro_use]
extern crate log;

#[macro_use]
extern crate common;
extern crate llvm as jit;
extern crate plan;
extern crate storage;

pub mod driver;
pub mod split;
pub mod task;
pub mod planner;
pub mod processor;

pub mod filter;
pub mod hash_join;
pub mod scan;

use std::collections::HashMap;

use common::err::{Result, Void};
use common::page::{Page, EncType};
use common::types::Ty;

use driver::DriverContext;

pub trait Executor
{
  fn init(&mut self) -> Void;
  fn need_input(&self) -> bool;
  fn add_input(&mut self, &Page) -> Void;
  fn next(&mut self) -> Result<&Page>;
  fn close(&mut self) -> Void;
}

pub trait ExecutorFactory
{
  fn create(&self, ctx: &DriverContext) -> Option<Box<Executor>>;

  fn types(&self) -> &Vec<Ty>;
}

pub struct NamedSchema<'a> {
  pub names: &'a [&'a str],
  pub types: &'a [&'a Ty],
  pub enc_types: &'a [&'a EncType],
}

impl<'a> NamedSchema<'a> {
  pub fn new(names: &'a [&'a str], types: &'a [&Ty], enc_types: &'a [&EncType]) -> NamedSchema<'a> {
    debug_assert_eq!(names.len(), types.len());
    debug_assert_eq!(types.len(), enc_types.len());

    NamedSchema {
      names: names,
      types: types,
      enc_types: enc_types,
    }
  }

  pub fn find_ids(&self, names: &[&str]) -> Vec<usize> {
    (0..self.names.len())
      .zip(self.names)
      .filter(|&(id, name)| names.contains(name))
      .map(|(id, name)| id)
      .collect::<Vec<usize>>()
  }

  pub fn to_map(&self) -> HashMap<&'a str, (usize, &'a Ty, &'a EncType)> {
    izip!(0..self.names.len(), self.names, self.types, self.enc_types)
      .map(|(id, n, t, e)| (*n, (id, *t, *e)))
      .collect::<HashMap<&str, (usize, &Ty, &EncType)>>()
  }
}
