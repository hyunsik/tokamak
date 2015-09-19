///
/// Delimited Text Scanner for comma/tab separated value formats
///

use alloc::heap;
use std::marker::PhantomData;
use std::mem;
use std::ptr::copy_nonoverlapping;
use std::result::Result;
use std::slice;

use common::constant::ROWBLOCK_SIZE;
use common::err::*;
use exec::Executor;
use io::stream::*;
use schema::Schema;
use schema::util::finds_target_indexes;
use rows::{copy_vectors, RowBlock, RowBlockWriter};
use rows::vrows::{BorrowedVRowBlock, HeapVRowBlock};
use types::*;
use util::str::{StrSlice,split_str_slice};

/// default buffer size
const BUF_SIZE: usize = 65536;

pub struct DelimTextScanner<'a> {
  // constants
  data_schema     : Schema,
  read_fields     : Option<Schema>,
  read_fields_idxs: Vec<usize>,

  line_delim  : u8,
  field_delim : u8,

  reader      : Box<StreamReader>,

  // variable
  readbuf_ptr      : *mut u8,           // readbuf
  read_len         : usize,             // length of read bytes
  consumed_len     : usize,             // actually consumed bytes length for line slices

  line_slices_idx  : usize,             // current line slice for reading
  line_slices_num  : usize,             // number of filled line slices
  line_slices_ptr  : *mut StrSlice,     // ptr for line slice array
  line_slices      : &'a mut [StrSlice],// line slice array

  fields_slices_ptr: *mut StrSlice,     // ptr for field slice array
  fields_slices    : &'a mut [StrSlice],// field slice array

  owned_rowblock   : HeapVRowBlock
}

impl<'a> DelimTextScanner<'a> {
  pub fn new(
        data_schema: Schema,
        read_fields: Option<Schema>,
        stream: Box<StreamReader>,
        field_delim: u8) -> DelimTextScanner<'a> {

    let read_fields_idxs = match read_fields {
      Some(ref s) => finds_target_indexes(&data_schema, s),
      None        => finds_target_indexes(&data_schema, &data_schema),
    };

    let mut line_slices_ptr = unsafe {
      heap::allocate(mem::size_of::<StrSlice>() * 1024, mem::min_align_of::<StrSlice>())
    } as *mut StrSlice;

    let mut line_slices: &mut [StrSlice] = unsafe {
      slice::from_raw_parts_mut(line_slices_ptr as *mut StrSlice, 1024)
    };

    let mut fields_slices_ptr = unsafe {
      heap::allocate(
        mem::size_of::<StrSlice>() * data_schema.size(),
        mem::min_align_of::<StrSlice>()
      )
    } as *mut StrSlice;

    let mut fields_slices: &mut [StrSlice] = unsafe {
      slice::from_raw_parts_mut(fields_slices_ptr as *mut StrSlice, data_schema.size())
    };

    let rowblock = HeapVRowBlock::new(
      match read_fields {
        Some(ref s) => s,
        None        => &data_schema
      }
    );

    DelimTextScanner {
      data_schema: data_schema,
      read_fields: read_fields,
      read_fields_idxs: read_fields_idxs,

      line_delim: '\n' as u8,
      field_delim: field_delim,

      reader: stream,

      readbuf_ptr: unsafe { heap::allocate(BUF_SIZE, 16) },
      consumed_len: 0,
      read_len: 0,

      line_slices_idx: 0,
      line_slices_num: 0,
      line_slices_ptr: line_slices_ptr,
      line_slices: line_slices,

      fields_slices_ptr: fields_slices_ptr,
      fields_slices: fields_slices,

      owned_rowblock: rowblock
    }
  }

  fn line_slices_num(&self) -> usize {
    self.line_slices_num
  }

  fn line_slices(&self) -> &[StrSlice] {
    self.line_slices
  }

  fn find_first_record_index(&self, bytes: &[u8]) -> Option<usize> {
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

  /// Return the last index of fields, which will be used for the following call.
  /// It will return a tuple consisting of found line number and last delim index.
  fn fill_line_slices(&mut self) {

    let mut line_num: usize = 0;
    let mut last_pos: usize = 0; // keep the start offset
    let mut cur_pos : usize = 0; // the current offset


    while (cur_pos < self.read_len && line_num < ROWBLOCK_SIZE) {
      // for each character
      let c: u8  = unsafe { *self.readbuf_ptr.offset(cur_pos as isize) };

      // check if the character is line delimiter
      if c == self.line_delim {

        // if found, set each StrSlice with the start offset and the current position
        self.line_slices[line_num].set_ptr(unsafe {self.readbuf_ptr.offset(last_pos as isize)});
        self.line_slices[line_num].set_len((cur_pos - last_pos) as i32);

        last_pos = cur_pos + 1; // to skip the delimiter character
        line_num = line_num + 1; // increase the line number
      }

      cur_pos = cur_pos + 1;
    }

    self.line_slices_num  = line_num; // read line counter
    self.consumed_len = last_pos;
    self.line_slices_idx = 0;
  }

  fn add_row(&mut self, row_idx: usize, split_num: usize) {

    //self.read_fields_idxs.iter().map(|x| x);

    let mut actual_id: usize;
    for x in 0..self.read_fields_idxs.len() {
      actual_id = self.read_fields_idxs[x];

      match self.data_schema.column(actual_id).data_ty().kind() {
        TyKind::Text => self.owned_rowblock.put_text(row_idx, x, &self.fields_slices[actual_id]),
        _ => {}
      }
    }
  }

  /// move the remain bytes into the header of buffer and fill the buffer.
  fn fill_readbuf(&mut self) -> Void {
    let remain_len = self.read_len - self.consumed_len;
    debug_assert!(remain_len >= 0, "remain length must be positive number.");

    // check if buffer should be compact.
    // If true, move the remain bytes into the head of the readbuf.
    if remain_len > 0 {
      unsafe {
        copy_nonoverlapping(
          self.readbuf_ptr.offset(self.consumed_len as isize),
          self.readbuf_ptr,
          remain_len);
      }
    }

    // fill readbuf
    self.read_len = try!(
      self.reader.read(
        unsafe {
          slice::from_raw_parts_mut(
            self.readbuf_ptr.offset(remain_len as isize),
            BUF_SIZE - remain_len
          )
        },
      )
    );

    self.read_len = self.read_len + remain_len;

    void_ok()
  }
}

impl<'a> Executor for DelimTextScanner<'a> {

  fn init(&mut self) -> Void {
    void_ok()
  }

  fn next<'b>(&'b mut self, rowblock: &mut BorrowedVRowBlock<'b>) -> TResult<bool> {

    // check if all data are consumed
    if (try!(self.reader.pos()) >= try!(self.reader.len())) {
      rowblock.set_row_num(0);
      return Ok(false); // return false to stop iteration
    }

    let mut row_num: usize = 0; // number of rows which are set to rowblock.
    let mut need_more_rows = true; // indicate whether rowblock is not full or not.
    let mut parse_result: (usize, usize);

    loop { // this loop continues until rowblock is filled or EOS

      if self.line_slices_idx >= self.line_slices_num { // need more line slices
        // compact and fill read buffer
        self.fill_readbuf();
        if self.read_len < 1 { // there is no readable buffer
          break;
        }
        self.fill_line_slices();
      }

      unsafe {
        let mut cur_line_idx = self.line_slices_idx;

        while cur_line_idx < self.line_slices_num && need_more_rows {

          // split each line slice into field slices
          parse_result = split_str_slice(
            &mut self.line_slices[cur_line_idx],
            self.fields_slices,
            self.field_delim
          );

          // the second value of parse result tuple is number of fields
          self.add_row(row_num, parse_result.1);

          cur_line_idx   = cur_line_idx + 1;
          row_num        = row_num + 1;
          need_more_rows = row_num < ROWBLOCK_SIZE;
        }

        // record parsed line index
        self.line_slices_idx = cur_line_idx;
      }

      if !need_more_rows {
        break;
      }
    } // outmost loop

    self.owned_rowblock.set_row_num(row_num);
    rowblock.set_row_num(row_num);
    //copy_vectors(&self.owned_rowblock, rowblock);
    for x in 0 .. self.owned_rowblock.column_num() {
      rowblock.set_vector(x, self.owned_rowblock.vector(x));
    }
    Ok(row_num > 0)
  }

  fn close(&mut self) -> Void {
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

#[test]
fn test_find_first_record_index() {
  let mut s = Schema::new();
  s.add_column("c1", *TEXT_TY);
  s.add_column("c2", *TEXT_TY);

  let fin = Box::new(FileInputStream::new("/home/hyunsik/tpch/lineitem/lineitem.tbl".to_string()));
  let s = DelimTextScanner::new(s, None, fin, '\n' as u8);

  assert_eq!(4, s.find_first_record_index("abc\nbb".as_bytes()).unwrap());
  assert_eq!(1, s.find_first_record_index("\nabc\nbb".as_bytes()).unwrap());
  assert_eq!(2, s.find_first_record_index("\r\nabc\nbb".as_bytes()).unwrap());
  assert!(s.find_first_record_index("aaaaabcabbb".as_bytes()).is_none());

}

#[test]
fn test_str_array() {
  let a: [&str;1024];

  //let mut slices: Vec<&str> = Vec::with_capacity(12);
  //unsafe { slices.set_len(12) };
  //slices.get_unchecked_mut(x)
  //slices[1] = "abc";

  let mut ptr: *mut &str = unsafe {
    heap::allocate(
      mem::size_of::<&str>() * 1024,
      mem::min_align_of::<&str>()
    ) as *mut &str
  };

  let mut slices: &mut [&str] = unsafe {
    slice::from_raw_parts_mut(ptr as *mut &str, 1024)
  };

  slices[0] = "abc";
}

#[test]
fn test_read_line_batch() {
  let mut schema = Schema::new();
  schema.add_column("c1", *TEXT_TY);
  schema.add_column("c2", *TEXT_TY);



  let mut fin = Box::new(FileInputStream::new("/Users/hyunsik/tpch/lineitem/lineitem.tbl".to_string()));
  assert!(fin.open().is_ok());
  let mut s = DelimTextScanner::new(schema.clone(), None, fin, '\n' as u8);
  assert!(s.init().is_ok());
    let mut rowblock = BorrowedVRowBlock::new(&schema);

  let mut sum = 0;
  //while(s.next(&mut rowblock).unwrap()) {
//    sum = sum + rowblock.row_num();
    //println!("acc: {}, rows: {}", sum, rowblock.row_num());
  //}
  {
    s.next(&mut rowblock);
  }
  { s.next(&mut rowblock); }
  sum = sum + rowblock.row_num();
  //s.next(&mut rowblock).unwrap();

  assert_eq!(6001216, sum);
}
  // let mut delim_indexes:Vec<usize> = Vec::new();
  // let r1 =
  //   s.next_line_indexes("abc\nbb\nabcdef\nabcd".as_ptr(), &mut delim_indexes);
  // assert_eq!(3, r1.0);
  // assert_eq!(13, r1.1);


//   delim_indexes.clear();

//   let r2 =
//     s.next_line_indexes("a\nb\nabcde\n".as_ptr(), &mut delim_indexes);
//   assert_eq!(3, r2.0);
//   assert_eq!(9, r2.1);
// }
