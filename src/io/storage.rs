use common::TypeClass;

pub trait TableSpace {
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