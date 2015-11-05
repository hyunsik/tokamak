/// Memory Table

use rows::*;
use types::Ty;

pub struct MemTable
{
	pages : Vec<Page>,
	types : Vec<Ty>,
	field_names: Vec<String>
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
}