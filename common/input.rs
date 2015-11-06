use std::rc::Rc;

use err::{Void, Result};
use types::Ty;
use session::Session;
use rows::Page;
use random_table::RandomTableGenerator;

pub trait InputSource {
  fn open    (&mut self) -> Void;
  fn has_next(&mut self) -> bool;
  fn next    (&mut self) -> Result<&Page>;
  fn close   (&mut self) -> Void;
}

pub type InputSourceFactory = Rc<Fn(&Session, &Vec<Ty>, usize) -> Box<InputSource>>;

pub fn get_factory(kind: &str) -> InputSourceFactory {
	match kind {
		"random" => {Rc::new(RandomTableGenerator::new)},
		_        => panic!("unknown storage: {}", kind)
			
	}
}