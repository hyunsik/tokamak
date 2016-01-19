//! MemTable is a in-memory table representation.
//!
//! MemTable is designed to be used to contain the result or for fastest processing.

use rustc_serialize::{Decoder, Decodable};

use session::Session;
use err;
use err::{Void, void_ok};
use page::{c_api, Page};
use types::Ty;

use util::collection::vec;

pub struct MemTable
{
	types   : Vec<Ty>,
	fields  : Vec<String>,

	pages   : Vec<Page>,
	row_num : usize
}

impl MemTable
{
	pub fn new(session: &Session, types: &[&Ty], fields: &[&str]) -> MemTable
	{
		debug_assert!(types.len() == fields.len());

		MemTable {
			types   : types.iter().map(|e| (*e).clone()).collect::<Vec<Ty>>(),
			fields  : fields.iter().map(|f| f.to_string()).collect::<Vec<String>>(),
			pages   : Vec::new(),
			row_num : 0
		}
	}
}

impl MemTable
{
	pub fn row_num(&self) -> usize { self.row_num }

	pub fn col_num(&self) -> usize { self.types.len() }

	pub fn types(&self) -> &Vec<Ty> {	&self.types }

	pub fn fields(&self) -> &Vec<String> { &self.fields }

	pub fn reader<'a, D: Decodable>(&'a self) -> DecodedRecords<'a, D>
	{
		DecodedRecords {
			pages_it: self.pages.iter(),
			cur_page: None,
			types   : &self.types,
			row_pos : 0,
			col_pos : 0,
			marker  : ::std::marker::PhantomData
		}
	}

	pub fn write(&mut self, page: &Page) -> Void
	{
		self.row_num += page.value_count() as usize;
		self.pages.push(page.copy());

		void_ok
	}

	pub fn close(&mut self) -> Void
	{
		self.pages.clear();
		void_ok
	}
}

use std::slice::Iter;

pub struct DecodedRecords<'a, D>
{
	pages_it: Iter<'a, Page>,
	cur_page: Option<&'a Page>,
	types   : &'a Vec<Ty>,
	row_pos : usize,
	col_pos : usize,
	marker: ::std::marker::PhantomData<D>,
}

impl<'a, D> DecodedRecords<'a, D>
{
	fn next_page(&mut self) {
		loop {
  		self.cur_page = self.pages_it.next();

  		if self.cur_page.is_none() {
  			break;
  		}

  		if self.cur_page.unwrap().value_count() == 0 {
 				continue;
			} else {
				break;
			}
		}
		self.row_pos  = 0;
	}

	#[inline(always)]
	fn validate(&self) {
		debug_assert!(self.row_pos  < self.cur_page.unwrap().value_count());
   	debug_assert!(self.col_pos  < self.types.len());
	}
}

impl<'a, D> Iterator for DecodedRecords<'a, D> where D: Decodable {
  type Item = DecodingResult<D>;

  fn next(&mut self) -> Option<DecodingResult<D>> {
  	println!(">>> row pos: {}", self.row_pos);

  	match self.cur_page {
  		Some(p) if self.row_pos >= p.value_count()  => self.next_page(),
  		None => self.next_page(),
  		_    => {}
  	};

  	if let Some(p) = self.cur_page {
  	  let r = Decodable::decode(self);
  	  self.row_pos += 1;

  	  Some(r)
 		} else {
 			None
 		}
  }
}

pub enum MemTableDecoderError
{
	TypeMismatch
}

pub type DecodingResult<T> = Result<T, MemTableDecoderError>;

impl<'a, D> Decoder for DecodedRecords<'a, D> {
    type Error = MemTableDecoderError;

    // Primitive types:
    fn read_nil(&mut self) -> Result<(), Self::Error>
    {
    	Ok(())
    }

    fn read_usize(&mut self) -> Result<usize, Self::Error>
    {
    	unimplemented!();
    }

    fn read_u64(&mut self) -> Result<u64, Self::Error>
    {
    	unimplemented!();
    }

    fn read_u32(&mut self) -> Result<u32, Self::Error>
    {
    	unimplemented!();
    }

    fn read_u16(&mut self) -> Result<u16, Self::Error>
    {
    	unimplemented!();
    }

    fn read_u8(&mut self) -> Result<u8, Self::Error>
    {
    	unimplemented!();
    }

    fn read_isize(&mut self) -> Result<isize, Self::Error>
    {
    	unimplemented!();
    }

    fn read_i64(&mut self) -> Result<i64, Self::Error>
    {
			self.validate();

    	let row_pos  = self.row_pos;
    	let col_pos  = self.col_pos;

    	let chunk = self.cur_page.unwrap().chunk(col_pos);
    	Ok(unsafe{c_api::read_i64_raw(chunk, row_pos)})
    }

    fn read_i32(&mut self) -> Result<i32, Self::Error>
    {
    	self.validate();

    	let row_pos  = self.row_pos;
    	let col_pos  = self.col_pos;

    	let chunk = self.cur_page.unwrap().chunk(col_pos);
    	Ok(unsafe{c_api::read_i32_raw(chunk, row_pos)})
    }

    fn read_i16(&mut self) -> Result<i16, Self::Error>
    {
//    	debug_assert!(self.page_pos < self.pages.len());
//    	debug_assert!(self.row_pos  < self.pages[self.page_pos].value_count());
//    	debug_assert!(self.col_pos  < self.types.len());
//
//    	let page_pos = self.page_pos;
//    	let row_pos  = self.row_pos;
//    	let col_pos  = self.col_pos;
//
//    	let mini_page = self.pages[page_pos].minipage(col_pos);
//    	Ok(mini_page.read_i16(row_pos))
			unimplemented!();
    }

    fn read_i8(&mut self) -> Result<i8, Self::Error>
    {
//    	let page_pos = self.page_pos;
//    	let row_pos  = self.row_pos;
//    	let col_pos  = self.col_pos;
//
//    	let mini_page = self.pages[page_pos].minipage(col_pos);
//    	Ok(mini_page.read_i8(row_pos))
			unimplemented!();
    }

    fn read_bool(&mut self) -> Result<bool, Self::Error>
    {
    	unimplemented!();
    }

    fn read_f64(&mut self) -> Result<f64, Self::Error>
    {
			self.validate();

    	let row_pos  = self.row_pos;
    	let col_pos  = self.col_pos;

    	let chunk = self.cur_page.unwrap().chunk(col_pos);
    	Ok(unsafe {c_api::read_f64_raw(chunk, row_pos)})
    }

    fn read_f32(&mut self) -> Result<f32, Self::Error>
    {
			self.validate();

    	let row_pos  = self.row_pos;
    	let col_pos  = self.col_pos;

    	let chunk = self.cur_page.unwrap().chunk(col_pos);
    	Ok(unsafe {c_api::read_f32_raw(chunk, row_pos)})
    }

    fn read_char(&mut self) -> Result<char, Self::Error>
    {
    	unimplemented!();
    }

    fn read_str(&mut self) -> Result<String, Self::Error>
    {
    	unimplemented!();
    }

    // Compound types:
    fn read_enum<T, F>(&mut self, name: &str, f: F) -> Result<T, Self::Error>
        where F: FnOnce(&mut Self) -> Result<T, Self::Error>
    {
      unimplemented!();
    }

    fn read_enum_variant<T, F>(&mut self, names: &[&str], f: F)
                               -> Result<T, Self::Error>
        where F: FnMut(&mut Self, usize) -> Result<T, Self::Error>
    {
      unimplemented!();
    }

    fn read_enum_variant_arg<T, F>(&mut self, a_idx: usize, f: F)
                                   -> Result<T, Self::Error>
        where F: FnOnce(&mut Self) -> Result<T, Self::Error>
    {
    	unimplemented!();
    }

    fn read_enum_struct_variant<T, F>(&mut self, names: &[&str], f: F)
                                      -> Result<T, Self::Error>
        where F: FnMut(&mut Self, usize) -> Result<T, Self::Error>
    {
    	unimplemented!();
    }

    fn read_enum_struct_variant_field<T, F>(&mut self,
                                            f_name: &str,
                                            f_idx: usize,
                                            f: F)
                                            -> Result<T, Self::Error>
        where F: FnOnce(&mut Self) -> Result<T, Self::Error>
    {
    	unimplemented!();
    }

    fn read_struct<T, F>(&mut self, s_name: &str, len: usize, f: F)
                         -> Result<T, Self::Error>
        where F: FnOnce(&mut Self) -> Result<T, Self::Error>
    {
    	unimplemented!();
    }

    fn read_struct_field<T, F>(&mut self,
                               f_name: &str,
                               f_idx: usize,
                               f: F)
                               -> Result<T, Self::Error>
        where F: FnOnce(&mut Self) -> Result<T, Self::Error>
    {
    	unimplemented!();
    }

    fn read_tuple<T, F>(&mut self, len: usize, f: F) -> Result<T, Self::Error>
        where F: FnOnce(&mut Self) -> Result<T, Self::Error>
    {
    	self.col_pos = 0;
      f(self)
    }

    fn read_tuple_arg<T, F>(&mut self, a_idx: usize, f: F)
                            -> Result<T, Self::Error>
        where F: FnOnce(&mut Self) -> Result<T, Self::Error>
    {
    	let r = f(self);
    	self.col_pos+=1;

    	r
    }

    fn read_tuple_struct<T, F>(&mut self, s_name: &str, len: usize, f: F)
                               -> Result<T, Self::Error>
        where F: FnOnce(&mut Self) -> Result<T, Self::Error>
    {
     	unimplemented!();
    }

    fn read_tuple_struct_arg<T, F>(&mut self, a_idx: usize, f: F)
                                   -> Result<T, Self::Error>
        where F: FnOnce(&mut Self) -> Result<T, Self::Error>
    {
    	unimplemented!();
    }

    // Specialized types:
    fn read_option<T, F>(&mut self, f: F) -> Result<T, Self::Error>
        where F: FnMut(&mut Self, bool) -> Result<T, Self::Error>
    {
    	unimplemented!();
    }

    fn read_seq<T, F>(&mut self, f: F) -> Result<T, Self::Error>
        where F: FnOnce(&mut Self, usize) -> Result<T, Self::Error>
    {
    	unimplemented!();
    }

    fn read_seq_elt<T, F>(&mut self, idx: usize, f: F) -> Result<T, Self::Error>
        where F: FnOnce(&mut Self) -> Result<T, Self::Error>
    {
    	unimplemented!();
    }

    fn read_map<T, F>(&mut self, f: F) -> Result<T, Self::Error>
        where F: FnOnce(&mut Self, usize) -> Result<T, Self::Error>
    {
      unimplemented!();
    }

    fn read_map_elt_key<T, F>(&mut self, idx: usize, f: F)
                              -> Result<T, Self::Error>
        where F: FnOnce(&mut Self) -> Result<T, Self::Error>
    {
      unimplemented!();
    }

    fn read_map_elt_val<T, F>(&mut self, idx: usize, f: F)
                              -> Result<T, Self::Error>
        where F: FnOnce(&mut Self) -> Result<T, Self::Error>
    {
      unimplemented!();
    }

    // Failure
    fn error(&mut self, err: &str) -> Self::Error
    {
    	unimplemented!();
    }
}