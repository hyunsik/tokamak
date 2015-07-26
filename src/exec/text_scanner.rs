use std::marker::PhantomData;
use std::mem;

use common::err::*;
use exec::Executor;
use io::stream::*;
use schema::Schema;
use rows::RowBlock;
use types::*;

pub struct DelimTextScanner<'a> {
  data_schema : Schema,
  read_fields : Option<Schema>,
  line_delim  : u8,
  field_delim : u8,
  reader      : Box<StreamReader>,
  marker      : PhantomData<&'a ()>
}

impl<'a> Executor for DelimTextScanner<'a> {
  fn init(&mut self) -> Void {
    self.reader.read();
    void_ok()
  }

  fn next(&self, rowblock: &mut RowBlock) -> Void {
    void_ok()
  }

  fn close(&self) -> Void {
    void_ok()
  }

  fn in_schema(&self) -> &Schema {
    &self.data_schema
  }

  fn out_schema(&self) -> &Schema {
    match self.read_fields {
      Some(ref s) => s,
      None => &self.data_schema
    }
  }
}

impl<'a> DelimTextScanner<'a> {
  pub fn new(
        data_schema: Schema,
        read_fields: Option<Schema>,
        stream: Box<StreamReader>, 
        field_delim: u8) -> DelimTextScanner<'a> {

    DelimTextScanner {
      data_schema: data_schema,
      read_fields: read_fields,

      line_delim: '\n' as u8,
      field_delim: field_delim,

      reader: stream,
      marker: PhantomData
    }
  }

  fn find_first_record_index(&self, text: &str) -> Option<usize> {
    let bytes : &[u8] = unsafe { mem::transmute(text) };

    let mut pos : usize = 0;
    let mut found : bool = false;

    for c in bytes {
      if self.line_delim == *c {
        found = true;
        break;
      }

      pos = pos + 1;
    }

    match found {
      true => Some(pos + 1),
      false => None
    }
  }

  /// Return the last index of fields, which will be used for the 
  /// following call
  fn next_line_indexes(&self, 
    text: &str, 
    delim_indexes: &mut Vec<usize>, 
    max_row_num: usize) -> (usize, usize) {

    let bytes : &[u8] = unsafe { mem::transmute(text) };
    let mut cur_delim_idx: usize = 0;
    let mut cur_pos: usize = 0;

    while (cur_pos < bytes.len() && cur_delim_idx < max_row_num) {
      let c: u8  = bytes[cur_pos];

      if c == self.line_delim {
        delim_indexes.push(cur_pos);
        cur_delim_idx = cur_delim_idx + 1;
      }

      cur_pos = cur_pos + 1;
    }

    let found_line_num = cur_delim_idx;
    let last_delim = if found_line_num > 0 
    { delim_indexes[found_line_num - 1] } else { -1 };

    (found_line_num, last_delim)
  }
}

#[test]
fn test_find_first_record_index() {
  let mut s = Schema::new();
  s.add_column("c1", *TEXT_TY);
  s.add_column("c2", *TEXT_TY);

  let fin = Box::new(FileInputStream::new("/home/hyunsik/tpch/lineitem/lineitem.tbl".to_string()));
  let s = DelimTextScanner::new(s, None, fin, '\n' as u8);

  assert_eq!(4, s.find_first_record_index("abc\nbb").unwrap());
  assert_eq!(1, s.find_first_record_index("\nabc\nbb").unwrap());
  assert_eq!(2, s.find_first_record_index("\r\nabc\nbb").unwrap());
  assert!(s.find_first_record_index("aaaaabcabbb").is_none());
}

#[test]
fn test_next_line_indxes() {
  let mut schema = Schema::new();
  schema.add_column("c1", *TEXT_TY);
  schema.add_column("c2", *TEXT_TY);

  let mut fin = Box::new(FileInputStream::new("/home/hyunsik/tpch/lineitem/lineitem.tbl".to_string()));
  let s = DelimTextScanner::new(schema, None, fin, '\n' as u8);

  let mut delim_indexes:Vec<usize> = Vec::new();
  let r1 = 
    s.next_line_indexes("abc\nbb\nabcdef\nabcd", &mut delim_indexes, 10);
  assert_eq!(3, r1.0);
  assert_eq!(13, r1.1);


  delim_indexes.clear();

  let r2 = 
    s.next_line_indexes("a\nb\nabcde\n", &mut delim_indexes, 10);
  assert_eq!(3, r2.0);
  assert_eq!(9, r2.1);
}