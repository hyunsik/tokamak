use common::err::{Error, Result, Void, void_ok};
use common::types::Ty;
use common::rows::{Page, PageBuilder};
use common::input::InputSource;

use storage::InputSourceFactory;

use super::{Executor, ExecutionContext, ExecutorFactory, Processor};

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
  fn create(&self, ctx: &ExecutionContext) -> Option<Box<Executor>> {
    None
  }
  
  fn types(&self) -> &Vec<Ty> {
    &self.types
  }
}

pub struct TableScanExec 
{
  page_builder: Box<PageBuilder>,
  input: Box<InputSource>,
  processor: Box<Processor>,
  
  cur_pos: u32
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
    
    try!(self.processor.process(
        read_page, 
        self.cur_pos, 
        read_page.value_count(), 
        &mut self.page_builder));
    
    Ok(self.page_builder.build())
  }
  
  fn close(&mut self) -> Void 
  {
    try!(self.input.close());
    void_ok
  }
}