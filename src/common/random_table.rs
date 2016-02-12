//! Random Data Generation Table
//!
//! It is designed for unit tests or users' prototypying.

use rand;

use itertools::Zip;

use err::{Result, Void, void_ok};
use session::Session;
use types::Ty;
use page::{Chunk, Page, ROWBATCH_SIZE, c_api};
use input::InputSource;

pub struct RandomTable {
  page: Page,
  write_fns: Vec<Box<Fn(&mut Chunk, usize)>>,
  row_num: usize, // number of rows to generate
  cur_pos: usize, // how many rows are generated so far?
}

impl RandomTable {
  pub fn new(session: &Session, types: &[&Ty], row_num: usize) -> Box<InputSource> {

    Box::new(RandomTable {
      page: Page::new(types, None),
      write_fns: types.iter()
                      .map(|ty| choose_random_fn(ty))
                      .collect::<Vec<Box<Fn(&mut Chunk, usize)>>>(),
      row_num: row_num,
      cur_pos: 0,
    })
  }
}

impl InputSource for RandomTable {
  fn open(&mut self) -> Void {
    void_ok
  }

  fn has_next(&mut self) -> bool {
    true
  }

  fn next(&mut self) -> Result<&Page> {
    println!("enter next");
    if self.cur_pos >= self.row_num {
      self.page.set_value_count(0);
      return Ok(&self.page);
    }

    // determine the row number to generate at this call
    let remain = self.row_num - self.cur_pos;
    let min = ::std::cmp::min(remain, ROWBATCH_SIZE);


    for (gen_fn, chunk) in Zip::new((self.write_fns.iter(), self.page.chunks_mut())) {
      (gen_fn)(chunk, min)
    }

    // move forward the position
    self.cur_pos += min;

    self.page.set_value_count(min);
    Ok(&self.page)
  }

  fn close(&mut self) -> Void {
    void_ok
  }
}

fn write_rand_for_i32(mp: &mut Chunk, rownum: usize) {
  unsafe {
    for pos in 0..rownum {
      c_api::write_i32_raw(mp, pos, rand::random::<i32>());
    }
  }
}

fn write_rand_for_i64(mp: &mut Chunk, rownum: usize) {
  unsafe {
    for pos in 0..rownum {
      c_api::write_i64_raw(mp, pos, rand::random::<i64>());
    }
  }
}

fn write_rand_for_f32(mp: &mut Chunk, rownum: usize) {
  unsafe {
    for pos in 0..rownum {
      c_api::write_f32_raw(mp, pos, rand::random::<f32>());
    }
  }
}

fn write_rand_for_f64(mp: &mut Chunk, rownum: usize) {
  unsafe {
    for pos in 0..rownum {
      c_api::write_f64_raw(mp, pos, rand::random::<f64>());
    }
  }
}

fn choose_random_fn(ty: &Ty) -> Box<Fn(&mut Chunk, usize)> {
  match ty.base() {
    "i32" => Box::new(write_rand_for_i32),
    "i64" => Box::new(write_rand_for_i64),
    "f32" => Box::new(write_rand_for_f32),
    "f64" => Box::new(write_rand_for_f64),
    _ => panic!("not supported type"),
  }
}
