//! MemTable is a in-memory table representation.
//!
//! MemTable is designed to be used to contain the result or for fastest processing. 

use rustc_serialize::Decodable;

use session::Session;
use err::{Result, Void, void_ok};
use rows::*;
use types::Ty;

pub struct MemTable
{
	types      : Vec<Ty>,
	field_names: Vec<String>,
	
	pages      : Vec<Page>,
	row_num    : usize
}

impl MemTable
{
	pub fn new(session: &Session, types: &Vec<Ty>, fields_name: &Vec<&str>) -> MemTable
	{
		MemTable {
			types      : types.clone(),
			field_names: ::util::str::to_owned_vec(fields_name),
			pages      : Vec::new(),
			row_num    : 0
		}
	} 
}

pub struct DecodedRecords<'a, D>
{
	pages: &'a Vec<Page>,
	types: &'a Vec<Ty>,
	marker: ::std::marker::PhantomData<D>,
}

impl<'a, D> Iterator for DecodedRecords<'a, D> where D: Decodable {
  type Item = Result<D>;
  
  fn next(&mut self) -> Option<Result<D>> {
  	unimplemented!()
  }
}

impl MemTable 
{
	pub fn row_num(&self) -> usize
	{
		self.row_num
	}
	
	pub fn col_num(&self) -> usize
	{
		self.types.len()
	}
	
	pub fn types(&self) -> &Vec<Ty>
	{
		&self.types
	}
	
	pub fn field_names(&self) -> &Vec<String>
	{
		&self.field_names
	}
	
	pub fn reader<'a, D: Decodable>(&'a self) -> DecodedRecords<'a, D> 
	{
		DecodedRecords {
			pages: &self.pages,
			types: &self.types,
			marker: ::std::marker::PhantomData
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