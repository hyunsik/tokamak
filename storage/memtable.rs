/// Memory Table

use common::rows::*;
use common::types::Ty;

pub struct MemTable
{
	pages : Vec<Page>,
	types : Vec<Ty>,
	fields: Vec<String>
}