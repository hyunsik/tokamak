///
/// Text Scanner
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
use rows::RowBlock;
use rows::vrows::BorrowedVRowBlock;
use types::*;
use util::str::{StrSlice,split_str_slice};

const BUF_SIZE: usize = 65536;

pub struct DelimTextScanner<'a> {
  data_schema : Schema,
  read_fields : Option<Schema>,
  line_delim  : u8,
  field_delim : u8,
  reader      : Box<StreamReader>,
  marker      : PhantomData<&'a ()>,

  // variable
  readbuf_ptr          : *mut u8,
  need_more_line_slices: bool, // parse lines from read buffer
  line_slices_idx      : usize, 
  read_line_num        : usize,    // number of read lines for each next
  line_slices_ptr      : *mut StrSlice,
  line_slices          : &'a mut [StrSlice],
  fields_slices_ptr    : *mut StrSlice,
  fields_slices        : &'a mut [StrSlice],  
  parsed_batch_len     : usize,
  read_len             : usize,  
}

impl<'a> DelimTextScanner<'a> {
  pub fn new(
        data_schema: Schema,
        read_fields: Option<Schema>,
        stream: Box<StreamReader>, 
        field_delim: u8) -> DelimTextScanner<'a> {

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

    DelimTextScanner {
      data_schema: data_schema,
      read_fields: read_fields,

      line_delim: '\n' as u8,
      field_delim: field_delim,

      reader: stream,
      marker: PhantomData,

      readbuf_ptr: unsafe { heap::allocate(BUF_SIZE, 16) },
      need_more_line_slices: true,
      line_slices_idx: 0,
      read_line_num: 0,
      line_slices_ptr: line_slices_ptr,
      line_slices: line_slices,
      fields_slices_ptr: fields_slices_ptr,
      fields_slices: fields_slices,
      parsed_batch_len: 0,
      read_len: 0  
    }
  }

  fn read_line_num(&self) -> usize {
    self.read_line_num
  }

  fn line_slices(&self) -> &[StrSlice] {
    self.line_slices
  }

  fn find_first_record_index(&self, bytes: &[u8]) -> Option<usize> {
    //let bytes : &[u8] = unsafe { mem::transmute(text) };

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

    self.read_line_num      = 0; // read line counter
    let mut last_pos: usize = 0; // keep the start offset
    let mut cur_pos : usize = 0; // the current offset


    while (cur_pos < self.read_len && self.read_line_num < ROWBLOCK_SIZE) {
      // for each character
      let c: u8  = unsafe { *self.readbuf_ptr.offset(cur_pos as isize) };

      // check if the character is line delimiter
      if c == self.line_delim {

        // if found, set each StrSlice with the start offset and the current position
        self.line_slices[self.read_line_num].set_ptr(unsafe {self.readbuf_ptr.offset(last_pos as isize)});
        self.line_slices[self.read_line_num].set_len((cur_pos - last_pos) as i32);

        last_pos = cur_pos + 1; // to skip the delimiter character
        self.read_line_num = self.read_line_num + 1; // increase the line number
      }

      cur_pos = cur_pos + 1;      
    }

    self.parsed_batch_len = last_pos;
    self.need_more_line_slices = false;    
    self.line_slices_idx = 0;
  }

  fn add_row(&mut self, row_idx: usize, split_num: usize) {
  }
  
  /// move the remain bytes into the header of buffer and fill the buffer.
  fn compact_and_refill_readbuf(&mut self) -> Void {
    //println!("compact enter!");
    if self.read_len < self.parsed_batch_len {
      println!("read_len: {}, parsed_batch_len: {}", self.read_len, self.parsed_batch_len);
    }
    let remain_len = self.read_len - self.parsed_batch_len;
    debug_assert!(remain_len >= 0, "remain length must be positive number.");
    
    // move the remain bytes into the head of the readbuf
    if remain_len > 0 {
      unsafe {    
        copy_nonoverlapping(
          self.readbuf_ptr.offset(self.parsed_batch_len as isize), 
          self.readbuf_ptr, 
          remain_len);
      }
    }
    
    // refill the readbuf
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
    self.need_more_line_slices = true;
    void_ok()
  }    
}

impl<'a> Executor for DelimTextScanner<'a> {  

  fn init(&mut self) -> Void {
    self.read_len = try!(self.reader.read(
      unsafe {slice::from_raw_parts_mut(self.readbuf_ptr, BUF_SIZE)}
    ));

    void_ok()
  }

  fn next(&mut self, rowblock: &mut RowBlock) -> TResult<bool> {   

    if (try!(self.reader.pos()) >= try!(self.reader.len())) {
      rowblock.set_row_num(0);
      return Ok(false);
    }

    let mut row_num: usize = 0;
    let mut field_parse_res: (usize, usize);
    let mut need_more_rows = true;
    loop { // this loop continues until rowblock is filled or 

      // parse lines in batch
      if self.need_more_line_slices {
        self.fill_line_slices();        
      }
      
      unsafe {      
        let mut line_idx = self.line_slices_idx;
        while line_idx < self.read_line_num && need_more_rows {
          
          // split each line slice into field slices
          field_parse_res = split_str_slice(
            &mut self.line_slices[line_idx],
            self.fields_slices,
            self.field_delim
          );
          
          
          line_idx = line_idx + 1;
          row_num = row_num + 1;          
          need_more_rows = row_num < ROWBLOCK_SIZE;
        }
        
        // record parsed line index
        self.line_slices_idx = line_idx;   
      }      
      
      if need_more_rows {
        // compact and fill read buffer 
        self.compact_and_refill_readbuf();        
                
        if self.read_len < 1 {
          break;
        }                
      } else {
        break;
      }
            
    } // outmost loop     
    
    println!("pos: {}, file_len: {}", try!(self.reader.pos()), try!(self.reader.len()));
    rowblock.set_row_num(row_num);
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

  //slices[0] = "abc";
}

#[test]
fn test_read_line_batch() {
  let mut schema = Schema::new();
  schema.add_column("c1", *TEXT_TY);
  schema.add_column("c2", *TEXT_TY);

  let mut rowblock = BorrowedVRowBlock::new(&schema);

  let mut fin = Box::new(FileInputStream::new("/Users/hyunsik/tpch/lineitem/lineitem.tbl".to_string()));
  assert!(fin.open().is_ok());
  let mut s = DelimTextScanner::new(schema, None, fin, '\n' as u8);
  assert!(s.init().is_ok());
  
  let mut sum = 0;
  while(s.next(&mut rowblock).unwrap()) {    
    sum = sum + rowblock.row_num();
    println!("acc: {}, rows: {}", sum, rowblock.row_num());
  }
  
  println!("{}", sum);
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