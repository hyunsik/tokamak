#[macro_use] extern crate common;
use common::session::Session;
use common::types::{BOOL, I8, I16, I32, I64, F32, F64};
use common::page::{c_api, Page, Chunk, RLEChunk, ROWBATCH_SIZE};
use common::input::InputSource;
use common::storage::RandomTable;

fn create_page() -> Page {
  Page::new(&[BOOL, I8, I16, I32, I64, F32, F64], None)
}

fn write(p: &Page) {
  unsafe {
    for x in 0..ROWBATCH_SIZE {
      let bool_chunk = p.chunk(0);
      c_api::write_i8_raw(bool_chunk, x, (x % 2) as i8);

      let i8_chunk = p.chunk(1);
      c_api::write_i8_raw(i8_chunk, x, (x % 7) as i8);

      let i16_chunk = p.chunk(2);
      c_api::write_i16_raw(i16_chunk, x, x as i16);

      let i32_chunk = p.chunk(3);
      c_api::write_i32_raw(i32_chunk, x, x as i32);

      let i64_chunk = p.chunk(4);
      c_api::write_i64_raw(i64_chunk, x, x as i64);

      let f32_chunk = p.chunk(5);
      c_api::write_f32_raw(f32_chunk, x, x as f32);

      let f64_chunk = p.chunk(6);
      c_api::write_f64_raw(f64_chunk, x, x as f64);
    }
  }
}

fn assert_page_contents(p: &Page) {
  unsafe {
    for x in 0..ROWBATCH_SIZE {
      let bool_chunk = p.chunk(0);
      assert_eq!((x % 2) as i8, c_api::read_i8_raw(bool_chunk, x));

      let i8_chunk = p.chunk(1);
      assert_eq!((x % 7) as i8, c_api::read_i8_raw(i8_chunk, x));

      let i16_chunk = p.chunk(2);
      assert_eq!(x as i16, c_api::read_i16_raw(i16_chunk, x));

      let i32_chunk = p.chunk(3);
      assert_eq!(x as i32, c_api::read_i32_raw(i32_chunk, x));

      let i64_chunk = p.chunk(4);
      assert_eq!(x as i64, c_api::read_i64_raw(i64_chunk, x));

      let f32_chunk = p.chunk(5);
      assert_eq!(x as f32, c_api::read_f32_raw(f32_chunk, x));

      let f64_chunk = p.chunk(6);
      assert_eq!(x as f64, c_api::read_f64_raw(f64_chunk, x));
    }
  }
}

#[test]
fn test_page() {
  let p = create_page();
  assert_eq!(7, p.chunk_num());
  assert_eq!(p.chunks().iter().map(|m| m.size).fold(0, |acc, s| acc + s), p.size());
}

#[test]
fn test_get_chunk() {
  let p = create_page();

  unsafe {
    for x in 0..p.chunk_num() {
      assert_eq!(&p.chunks()[x] as *const Chunk, c_api::get_raw_chunk(&p, x));
      assert_eq!(p.chunk_ptr(x), c_api::get_raw_chunk(&p, x));
    }
  }
}

#[test]
fn test_page_copy() {
  let mut p = create_page();
  write(&p);
  p.set_value_count(ROWBATCH_SIZE);
  assert_page_contents(&p);

  let copy = p.copy();
  assert_page_contents(&copy);

  assert_eq!(p.size(), copy.size());
  assert_eq!(p.chunk_num(), copy.chunk_num());
  assert_eq!(p.value_count(), copy.value_count());
}


#[test]
pub fn test_project()
{
  let session = Session;
  let mut gen = RandomTable::new(&session, &[I32, F32, F32], 5);

  let page      = gen.next().unwrap();
  let projected = page.project(&vec![1,2]);

  unsafe {
    for x in 0 .. 5 {
      assert_eq!(c_api::read_f32_raw(page.chunk(1), x), c_api::read_f32_raw(projected[0], x));
      assert_eq!(c_api::read_i32_raw(page.chunk(2), x), c_api::read_i32_raw(projected[1], x));
    }
  }
}

#[test]
pub fn test_rle_chunk() {
  let chunk: RLEChunk = unsafe { c_api::random_rle_chunk() };
  let mut idx: usize = 0;
  for i in 0..10 {
    for j in 0..(i + 1) {
      unsafe {
        assert_eq!(i * 10i32, c_api::read_i32_rle(&chunk, idx));
      }
      idx += 1;
    }
  }
}
