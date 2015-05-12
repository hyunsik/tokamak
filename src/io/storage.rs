use std::collections::HashMap;
use std::marker::PhantomData;
use std::sync::Mutex;

use common::err::{Error, TResult, Void};
use common::TypeClass;

//fn default_space_mapper()

pub trait TableSpace<'a> {
	fn uri(&self) -> &'a str;

	fn create_table(&self);

	fn drop_table(&self);

	fn rename_table(&self);

	fn truncate_table(&self);

	fn available_capacity(&self) -> u64;

	fn total_capacity(&self) -> u64;
}

pub trait DataFormat {
	fn name(&self) -> String;

	fn splittable() -> bool;

	fn compression_support() -> bool;

	fn random_accessable() -> bool;

	fn indexable() -> bool;

	fn supported_types() -> Vec<TypeClass>;
}

pub trait Storage {
	fn uri (&self) -> String;	

	fn available_capacity(&self) -> u64;

	fn total_capacity(&self) -> u64;
}

pub trait BlockStorage: Storage {
	fn split(&self, url: String, size : u64);

	fn list_files(url : String);
}

pub struct StorageManager<'a> {
	map: Mutex<HashMap<String, Box<TableSpace<'a>>>>,

	//space_mapper: fn(scheme: &str) -> TableSpace,
	marker: PhantomData<&'a ()>
}

impl<'a> StorageManager<'a> {

	// pub fn new() -> StorageManager {

	// }

	// pub fn new_with_handler(scheme_type_mapper) -> StorageManager {

	// }

	// pub fn get_tablespace(&'a self) -> TResult<&'a TableSpace> {
	// 	Err(Error::Unknown)
	// }
}