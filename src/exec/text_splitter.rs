use common::err::Error;

/// Field Delimiter Trait
pub trait FieldSplitterTrait {
  fn parse<'a>(
    &self,
    line: &'a [u8],
    fields: &mut Vec<&'a [u8]>,
    fields_num: u32) -> Result<u32, Error>;
}

pub struct FieldSplitter<R> {
  pub instance: R
}

impl<R: FieldSplitterTrait> FieldSplitter<R> {
  fn parse<'a>(
    &self,
    line: &'a [u8],
    fields: &mut Vec<&'a [u8]>,
    fields_num: u32) -> Result<u32, Error> {

    self.instance.parse(line, fields, fields_num)
  }
}

pub struct SingleCharSplitter {
  delim: u8
}

impl FieldSplitterTrait for SingleCharSplitter {

  /// Return value - the number of parsed fields
  fn parse<'a>(
    &self,
    line: &'a [u8],
    fields: &mut Vec<&'a [u8]>,
    fields_num: u32) -> Result<u32, Error> {

    let y = self.delim;

    for x in line {
      match x {
      x if *x == y => { println!("Matched"); }
      _ => { println!("unmatched"); }
      }
    }

    Ok(1)
  }
}

#[test]
fn test_SingleCharSplitter() {

  let splitter = SingleCharSplitter {delim: ',' as u8};
  let s = "abc,def".to_string();
  let bytes = s.as_bytes();

  let mut slices: Vec<&[u8]> = Vec::with_capacity(2);
  splitter.parse(bytes, &mut slices, 2);
}
