use std::rc::Rc;
use std::cell::RefCell;

use data_structures::fnv::FnvHashMap;
use syntax::ast;

use syntax::parse::token::IdentInterner;

pub struct PackageMetadata {
  pub name: String
}

pub struct CStore {
  metas: RefCell<FnvHashMap<ast::CrateNum, Rc<PackageMetadata>>>,
  pub intr: Rc<IdentInterner>,
}

impl CStore {
    pub fn new(intr: Rc<IdentInterner>) -> CStore {
      CStore {
        metas: RefCell::new(FnvHashMap()),
        intr: intr
      }
    }
}