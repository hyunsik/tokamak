pub mod cstore {
  use std::any::Any;

  /// A store of Rust crates, through with their metadata
  /// can be accessed.
  ///
  /// The `: Any` bound is a temporary measure that allows access
  /// to the backing `rustc_metadata::cstore::CStore` object. It
  /// will be removed in the near future - if you need to access
  /// internal APIs, please tell us.
  pub trait CrateStore<'tcx> : Any {
  }
}