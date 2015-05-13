use std::fs::File;
use std::io::Read;
use std::mem;
use std::marker::PhantomData;
use std::option::Option;
use std::str;

use common::err::*;

pub struct Buf<'a> {
  ptr: &'a [u8],
  len: usize
}

impl<'a> Buf<'a> {

  #[inline]
  pub fn as_slice(&self) -> &'a [u8] {
    self.ptr
  }

  #[inline]
  pub fn len(&self) -> usize {
    self.len
  }
}

pub trait StreamReader<'a> {
  fn open(&mut self) -> Void;
  fn read(&'a mut self) -> TResult<Buf<'a>>;  
  fn close(&self) -> Void;

  fn len(&self) -> TResult<u64>;
  fn pos(&self) -> TResult<u64>;
  fn remain(&self) -> TResult<u64>;
  fn eos(&self) -> TResult<bool>;
}

pub static DEFAULT_BUF_SIZE:usize = 4096;

pub struct FileInputStream<'a> {
  path: String,
  buf_size: usize,

  buf: Vec<u8>,

  file: Option<File>,
  len: u64,
  pos: u64,

  marker: PhantomData<&'a ()>,
}

impl<'a> FileInputStream<'a> {
  pub fn new(path: String) -> FileInputStream<'a> {
    FileInputStream::new_with_bufsize(path, DEFAULT_BUF_SIZE)
  }

  pub fn new_with_bufsize(path: String, buf_size: usize) -> FileInputStream<'a> {
    FileInputStream {
      path: path,
      buf_size: buf_size,

      buf: Vec::with_capacity(buf_size), 
      marker: PhantomData,

      file: None,
      len: 0,
      pos: 0
    }
  }
}

impl<'a> StreamReader<'a> for FileInputStream<'a> {  

  fn open(&mut self) -> Void {    

    match File::open(&(self.path)) {
      Ok(file) => {
        self.file = Some(file);
        self.len = try!(self.file.as_mut().unwrap().metadata()).len();
        self.pos = 0;

        void_ok()
      },
      Err(e) => void_ok()
    }    
  }

  #[inline]
  fn read(&'a mut self) -> TResult<Buf<'a>>  {
    debug_assert!(self.file.is_some(), "File must be opened before calling read()");    
    
    match self.file.as_mut().unwrap().read(&mut self.buf) {
      Ok(read_len) => {
        self.pos = self.pos + read_len as u64;
        Ok(Buf {ptr: &self.buf, len: read_len})
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

  let mut fin = FileInputStream::new("/home/hyunsik/tpch/lineitem/lineitem.tbl".to_string());
  let mut reader:&mut StreamReader = &mut fin;
  
  let r = reader.open();
  assert!(r.is_ok());

  let read = reader.read();
  assert!(read.is_ok());

  let buf = read.ok().unwrap();
  println!("{}", buf.len());  
  let s = str::from_utf8(buf.as_slice()).ok().unwrap();
  println!("{}", s);
}