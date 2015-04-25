use common::Error;
use exec::Executor;
use tuple::VecRowBlockTrait;

// void ParseFields(StringPiece *line, StringPiece fields[], int fields_num, int &actual_fields_num);

#[derive(Debug)]
pub struct DelimTextScanner {
  path: String
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

impl DelimTextScanner {  
  fn find_first_record_index(&self, line: &str) -> Result<usize, Error> {
    Ok(0)
  }

  fn next_line_delim_index(&self, buf: &str) -> Result<usize, Error> {
    Ok(0)
  }
}