use common::Error;
use exec::Executor;
use tuple::VecRowBlockTrait;
use url::Url;

#[derive(Debug)]
pub struct DelimTextScanner {
  path: Url
}

impl Executor for DelimTextScanner {
  fn init(&self) -> Result<bool, Error> {
    Ok(true)
  }
  fn next(&self, rowblock: &mut VecRowBlockTrait) -> Result<bool, Error> {
    Ok(true)
  }
  fn close(&self) -> Result<bool, Error> {
    Ok(true)
  }
}