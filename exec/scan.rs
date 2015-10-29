use common::err::{Error, Result, Void, void_ok};
use common::types::Ty;
use common::rows::{Page, PageBuilder};
use common::input::InputSource;

use super::{ExecutionContext, ExecutorFactory};

use super::{Executor, Processor};

pub struct TableScanExecFactory<'a> 
{
  schema: Vec<&'a Ty>
}

impl<'a> TableScanExecFactory<'a> 
{
  pub fn new(schema: Vec<&'a Ty>) -> TableScanExecFactory<'a> 
  {
    TableScanExecFactory { 
      schema: schema 
    }
  }
}

impl<'a> ExecutorFactory for TableScanExecFactory<'a> 
{
  fn create(&self, ctx: &ExecutionContext) -> Option<Box<Executor>> {
    None
  }
  
  fn schema(&self) -> &Vec<&Ty> {
    &self.schema
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