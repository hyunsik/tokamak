use std::collections::HashMap;
use std::marker::PhantomData;
use std::sync::{Arc, Mutex};

use common::err::*;
use common::TypeClass;

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
pub fn default_space_handlers<'b> (url: &Url) -> Box<TableSpace<'b>> {

	match url.scheme.as_str() {
		"file" => Box::new( LocalFS::new(&url.serialize()) ) ,
		_ => panic!("No supported: {}", url.serialize())
	}
}

pub trait TableSpace<'a> {
	fn uri(&'a self) -> &'a str;

	fn format(&self) -> Void;

	fn create_table(&self) -> Void ;

	fn drop_table(&self, purge: bool) -> Void;

	fn rename_table(&self) -> Void;

	fn truncate_table(&self) -> Void;

	fn available_capacity(&self) -> u64;

	fn total_capacity(&self) -> u64;
}

pub struct LocalFS {
	url: String
}

impl LocalFS {
	pub fn new(url: &str) -> LocalFS {
		LocalFS {url: url.to_owned()}
	}
}

impl<'a> TableSpace<'a> for LocalFS {
	fn uri(&'a self) -> &'a str {
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
	space_handler: fn(uri: &'a Url) -> Box<TableSpace>,
	space_map: Mutex<HashMap<String, Box<TableSpace<'a>>>>,
	url_parser: UrlParser<'a>,
	marker: PhantomData<&'a ()>
}

impl<'a> StorageManager<'a> {

	pub fn new() -> StorageManager<'a> {
		StorageManager::new_with_handler(default_space_handlers)
	}

	pub fn new_with_handler(space_handler: fn(uri: &Url) -> Box<TableSpace>) -> StorageManager<'a> {
		
		let mut url_parser = UrlParser::new();
    url_parser.scheme_type_mapper(hdfs_scheme_handler);

		StorageManager {
			space_handler: space_handler, 
			space_map: Mutex::new(HashMap::new()),
			url_parser: url_parser,
			marker: PhantomData
		}
	}

	pub fn get_space_url(&'a self, url: &str) -> Option<String> {		
		let map = self.space_map.lock().unwrap();
		let mut it = map.keys().filter(|&u| u == url);

		match it.next() {
			Some(u) => Some(u.to_owned()),
			None => None
		}
	}

	pub fn get_space(&'a self, url: &str) -> TResult<&'a TableSpace> {

		let mut guard = match self.space_map.lock() {
			Ok(guard) => guard,
			Err(e) => { error!("{}", e); panic!("poisoned") }
		};

		Err(Error::Unknown)
	}
}