use std::cell::UnsafeCell;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::marker::PhantomData;
use std::sync::{Arc, Mutex};

use common::err::*;
use common::TypeClass;

use exec::Executor;

use url::{Url, UrlParser, SchemeType, whatwg_scheme_type_mapper};



/// for HDFS URL scheme (i.e., hdfs://)
fn hdfs_scheme_handler(scheme: &str) -> SchemeType {
  match scheme {
    "file" => SchemeType::FileLike,
    "hdfs" => SchemeType::Relative(50070),
    _ => whatwg_scheme_type_mapper(scheme)
  }
}

/// storage handler mapper for StorageManager
pub fn default_space_handlers<'a>(url: Url) -> TResult<Box<TableSpace<'a>>> {
	match url.scheme.as_str() {
		"file" => Ok(Box::new(LocalFS::new(&url.serialize()))),
		_ => {
			Err(Error::UnsupportedTableSpace)
		}
	}
}

pub trait TableSpace<'a> {
	fn url(&'a self) -> &'a str;

	fn format(&self) -> Void;

	fn create_table(&self) -> Void ;

	fn drop_table(&self, purge: bool) -> Void;

	fn rename_table(&self) -> Void;

	fn truncate_table(&self) -> Void;

	fn available_capacity(&self) -> u64;

	fn total_capacity(&self) -> u64;

	fn new_scanner(&self, url: &str) -> TResult<Box<Executor>>;

	fn new_appender(&self, url: &str) -> TResult<Box<Executor>>;
}

pub struct LocalFS {
	pub url: String
}

impl LocalFS {
	pub fn new(url: &str) -> LocalFS {
		LocalFS {url: url.to_owned()}
	}
}

impl<'a> TableSpace<'a> for LocalFS {
	fn url(&'a self) -> &'a str {
		self.url.as_str()
	}

	fn format(&self) -> Void {
		void_ok()
	}

	fn create_table(&self) -> Void {		
		void_ok()
	}

	fn drop_table(&self, purge: bool) -> Void {		
		void_ok()
	}

	fn rename_table(&self) -> Void {
		void_ok()
	}

	fn truncate_table(&self) -> Void {
		void_ok()
	}

	fn available_capacity(&self) -> u64 {
		0
	}

	fn total_capacity(&self) -> u64 {		
		0
	}

	fn new_scanner(&self, url: &str, format_type: &str) -> TResult<Box<Executor>> {
		Err(Error::Unimplemented)
	}

	fn new_appender(&self, url: &str, format_type: &str) -> TResult<Box<Executor>> {
		Err(Error::Unimplemented)
	}
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

/// Manages TableSpaces
pub struct StorageManager<'a> {
	space_handler: fn(uri: Url) -> TResult<Box<TableSpace<'a>>>,
	space_map: UnsafeCell<HashMap<String, Box<TableSpace<'a>>>>,
	lock: Mutex<()>,
	url_parser: UrlParser<'a>,
	marker: PhantomData<&'a ()>
}

impl<'a> StorageManager<'a> {

	pub fn new() -> StorageManager<'a> {
		StorageManager::new_with_handler(default_space_handlers)
	}

	pub fn new_with_handler(space_handler: fn(uri: Url) -> TResult<Box<TableSpace<'a>>>) -> StorageManager<'a> {
		
		let mut url_parser = UrlParser::new();
    url_parser.scheme_type_mapper(hdfs_scheme_handler);

		StorageManager {
			space_handler: space_handler, 
			space_map: UnsafeCell::new(HashMap::new()),
			lock: Mutex::new(()),
			url_parser: url_parser,
			marker: PhantomData
		}
	}

	#[allow(unused_must_use)]
	pub fn get_space(&'a self, url: &str) -> TResult<&'a TableSpace> {
		self.lock.lock();

		unsafe {
			let key = url.to_owned();
			match (*self.space_map.get()).entry(key) {				
				Entry::Vacant(entry) => {
					let res = self.url_parser.parse(url);					
					if res.is_err() {
						panic!("Invalid space url: {}", url)
					}

					let parsed_url = res.unwrap();
					let handler = try!((self.space_handler)(parsed_url));
					Ok(&(**entry.insert(handler)))
				},
				Entry::Occupied(entry) => {
					let x: &Box<TableSpace<'a>> = entry.get();
					Err(Error::Unknown)					
				}
			}
		}
	}
}