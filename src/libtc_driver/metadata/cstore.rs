use std::rc::Rc;

use syntax::parse::token::IdentInterner;

pub struct CStore {
  pub intr: Rc<IdentInterner>,
}

impl CStore {
    pub fn new(intr: Rc<IdentInterner>) -> CStore {
      CStore {
        intr: intr
      }
    }
}