use std::cell::RefCell;
use std::rc::Rc;

use fnv::FnvHashMap;

use common::codespan::{self, Span};
use parser::ast::NodeId;

pub type PackageId = u32;

pub struct ImportedFileMap {
  pub filemap: Rc<codespan::FileMap>
}

pub struct PackageMetadata {
  pub name: String,

  // span information used to import this package
  pub span: Span,
  pub codemap_import_info: RefCell<Vec<ImportedFileMap>>,
}

pub struct PackageStore {
  metas: RefCell<FnvHashMap<PackageId, Rc<PackageMetadata>>>,
  extern_pkg_map: FnvHashMap<NodeId, PackageId>,
}