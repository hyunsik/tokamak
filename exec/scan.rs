use common::err::{Void, TResult, Error, void_ok};
use common::rows::{Page, PageBuilder};
use storage::InputSource;

use super::Executor;

pub struct ScanExec 
{
  page_builder: Box<PageBuilder>,
  input: Box<InputSource>
}

impl Executor for ScanExec 
{
  fn init(&mut self) -> Void
  {
    void_ok()
  }
  
  fn need_input(&self) -> bool { true }
  
  fn add_input(&mut self, page: &Page) -> Void
  {
    void_ok()    
  }
  
  fn get_output(&mut self) -> TResult<&Page>
  {
    Err(Error::InternalError)
  }
  
  fn close(&mut self) -> Void 
  {
    void_ok()
  }
}