use common::err::{Error, Result, Void, void_ok};
use common::input::{InputSource, InputSourceFactory};
use common::types::Ty;
use common::page::Page;

use driver::DriverContext;
use super::{Executor, ExecutorFactory};
use processor::Processor;

pub struct TableScanExecFactory
{
  pub types: Vec<Ty>,
  pub source_factory: InputSourceFactory
}

impl<'a> TableScanExecFactory
{
  pub fn new(
  	types: &Vec<Ty>,
  	source_factory: InputSourceFactory) -> TableScanExecFactory
  {
    TableScanExecFactory {
      types: types.clone(),
      source_factory: source_factory
    }
  }
}

impl<'a> ExecutorFactory for TableScanExecFactory
{
  fn create(&self, ctx: &DriverContext) -> Option<Box<Executor>> {
    None
  }

  fn types(&self) -> &Vec<Ty> {
    &self.types
  }
}

pub struct TableScanExec
{
  output: Page,
  input: Box<InputSource>,
  processor: Box<Processor>,

  cur_pos: usize
}

impl Executor for TableScanExec
{
  fn init(&mut self) -> Void
  {
    try!(self.input.open());
    void_ok
  }

  fn need_input(&self) -> bool { true }

  fn add_input(&mut self, page: &Page) -> Void
  {
    void_ok
  }

  fn next(&mut self) -> Result<&Page>
  {
    let read_page: &Page = try!(self.input.next());

    /*
    try!(self.processor.process(
        read_page,
        &mut self.output));
       */

    Ok(&self.output)
  }

  fn close(&mut self) -> Void
  {
    try!(self.input.close());
    void_ok
  }
}