//! MemTable is a in-memory table representation.
//!
//! MemTable is designed to be used to contain the result or for fastest processing. 

use rustc_serialize::Decodable;

use err::{Result, Void};
use rows::*;
use types::Ty;

pub struct MemTable
{
	pages : Vec<Page>,
	types : Vec<Ty>,
	field_names: Vec<String>
}

pub struct DecodedRecords<'a, D>
{
	pages: &'a Vec<Page>,
	marker: ::std::marker::PhantomData<D>,
}

impl MemTable 
{
	pub fn row_num(&self) -> usize
	{
		0
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
			marker: ::std::marker::PhantomData
		}
	}
	
	pub fn write(page: &Page) -> Void
	{
		unimplemented!()
	}  
}