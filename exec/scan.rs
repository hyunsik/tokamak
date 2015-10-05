use common::err::{Void, TResult, Error, void_ok};
use common::rows::{Page, PageBuilder};
use storage::InputSource;

use super::{Executor, Processor};

pub struct ScanExec 
{
  page_builder: Box<PageBuilder>,
  input: Box<InputSource>,
  processor: Box<Processor>,
  
  cur_pos: u32
}

impl Executor for ScanExec 
{
  fn init(&mut self) -> Void
  {
    try!(self.input.open());
    void_ok()
  }
  
  fn need_input(&self) -> bool { true }
  
  fn add_input(&mut self, page: &Page) -> Void
  {
    void_ok()    
  }
  
  fn next(&mut self) -> TResult<&Page>
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
    void_ok()
  }
}