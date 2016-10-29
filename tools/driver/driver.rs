#[macro_use] extern crate log;
extern crate env_logger;
extern crate flang_driver as driver;
extern crate term;

use std::env;
use std::process;

use driver::ErrorDestination;

pub fn main() {
  env_logger::init().unwrap();

  let exit_code = driver::run_driver(
    std::env::args().collect(),
    env::current_dir().unwrap(),
    ErrorDestination::Stderr
  );

  process::exit(exit_code);
}