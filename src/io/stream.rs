use std::fs::File;
use std::io::Read;
use std::mem;
use std::marker::PhantomData;
use std::option::Option;

use common::err::*;

pub struct Buffer<'a> {
  ptr: &'a [u8],
  len: usize
}

pub trait Readable<'a> {
  fn open(&mut self) -> Void;
  fn read(&'a mut self) -> TResult<Buffer<'a>>;  
  fn close(&self) -> Void;

  fn len(&self) -> TResult<u64>;
  fn pos(&self) -> TResult<u64>;
  fn remain(&self) -> TResult<u64>;
  fn eos(&self) -> TResult<bool>;
}

pub static DEFAULT_BUF_SIZE:usize = 4096;

pub struct FileInputStream<'a> {
  path: String,
  buf: [u8;4096],
  marker: PhantomData<&'a ()>,

  file: Option<File>,
  len: u64,
  pos: u64
}

impl<'a> FileInputStream<'a> {
  pub fn new(path: &str) -> FileInputStream {
    FileInputStream {
      path: path.to_owned(), 
      //buf: Vec::with_capacity(DEFAULT_BUF_SIZE), 
      buf: unsafe { mem::zeroed() }, 
      marker: PhantomData,

      file: None,
      len: 0,
      pos: 0
    }
  }
}

impl<'a> Readable<'a> for FileInputStream<'a> {  

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
  fn read(&'a mut self) -> TResult<Buffer<'a>>  {
    debug_assert!(self.file.is_some(), "File must be opened before calling read()");    
    
    match self.file.as_mut().unwrap().read(&mut self.buf) {
      Ok(read_len) => {
        self.pos = self.pos + read_len as u64;
        Ok(Buffer {ptr: &self.buf, len: read_len})
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