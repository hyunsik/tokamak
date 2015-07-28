use std::fs::File;
use std::io::{Read, Seek, SeekFrom};
use std::mem;
use std::marker::PhantomData;
use std::option::Option;
use std::str;

use common::err::*;

pub trait StreamReader {
  fn open(&mut self) -> Void;
  fn read(&mut self, &mut [u8]) -> TResult<usize>;  
  fn close(&self) -> Void;

  fn len(&self) -> TResult<u64>;
  fn pos(&self) -> TResult<u64>;
  fn remain(&self) -> TResult<u64>;
  fn eos(&self) -> TResult<bool>;
}

pub static DEFAULT_BUF_SIZE:usize = 4096;

pub struct FileInputStream<'a> {
  path: String,
  file: Option<File>,
  len: u64,
  pos: u64,

  marker: PhantomData<&'a ()>,
}

impl<'a> FileInputStream<'a> {
  pub fn new(path: String) -> FileInputStream<'a> {
    FileInputStream {
      path: path, 
      marker: PhantomData,

      file: None,
      len: 0,
      pos: 0
    }
  }
}

impl<'a> StreamReader for FileInputStream<'a> {  

  fn open(&mut self) -> Void {    

    match File::open(&(self.path)) {
      Ok(mut file) => {        
        file.seek(SeekFrom::Start(0u64));
        self.len = try!(file.metadata()).len();
        
        self.file = Some(file);
        self.pos = 0;       

        void_ok()
      },
      Err(e) => void_ok()
    }    
  }

  #[inline]
  fn read(&mut self, buf: &mut [u8]) -> TResult<usize>  {
    debug_assert!(self.file.is_some(), "File must be opened before calling read()");    
    
    match self.file.as_mut().unwrap().read(buf) {
      Ok(read_len) => {
        self.pos = self.pos + read_len as u64;
        Ok(read_len)
      },
      Err(e) => Err(Error::Unknown)
    }
  }

  #[inline]
  fn close(&self) -> Void {
    void_ok()
  }

  #[inline]
  fn len(&self) -> TResult<u64> {
    Ok(self.len)
  }

  #[inline]
  fn pos(&self) -> TResult<u64> {
    Ok(self.pos)
  }

  #[inline]
  fn remain(&self) -> TResult<u64> {
    Ok(self.len - self.pos)
  }

  #[inline]
  fn eos(&self) -> TResult<bool> {
    Ok(true)
  }
}

#[test]
pub fn test_file_read() {

  let mut fin = FileInputStream::new("/Users/hyunsik/tpch/lineitem/lineitem.tbl".to_string());
  let mut reader:&mut StreamReader = &mut fin;
  
  assert!(reader.open().is_ok());

  println!("file len: {}", reader.len().unwrap());

  let mut buf: [u8;4096] = unsafe {mem::zeroed()};
  let read_len = reader.read(&mut buf);
  assert!(read_len.is_ok());

  let len = read_len.ok().unwrap();
  println!("len: {}, pos: {}", len, reader.pos().unwrap());  
  let s = str::from_utf8(&buf).ok().unwrap();
  println!("contents: {}", s);
  reader.close();
}