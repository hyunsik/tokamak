use std::fmt::Write;

use common::page::{c_api, Page};
use common::types::Ty;

pub trait RowPrinter {
  fn write(&[&Ty], &Page, &mut String);
}


pub struct ColumnarRowPrinter;

macro_rules! write_value(
  ($ty:ident, $name:ident) => (
    Ty::$ty => { write!(buf, "{}", unsafe { c_api::$name(page.chunk(col_idx), row_idx) }); }
  );
);

impl RowPrinter for ColumnarRowPrinter {
  fn write(tys: &[&Ty], page: &Page, buf: &mut String) {
    for row_idx in 0..page.value_count() {

      // newline for rows
      if row_idx != 0 {
        write!(buf, "\n");
      }

      for (col_idx, ty) in izip!(0..tys.len(), tys.iter()) {

        // comma for column values
        if col_idx != 0 {
          write!(buf, ",");
        }

        match **ty {
          Ty::Bool => write!(buf, "{}", unsafe { c_api::read_i8_raw(page.chunk(col_idx), row_idx) }),
          Ty::I8 => write!(buf, "{}", unsafe { c_api::read_i8_raw(page.chunk(col_idx), row_idx) }),
          Ty::I16 => write!(buf, "{}", unsafe { c_api::read_i16_raw(page.chunk(col_idx), row_idx) }),
          Ty::I32 => write!(buf, "{}", unsafe { c_api::read_i32_raw(page.chunk(col_idx), row_idx) }),
          Ty::I64 => write!(buf, "{}", unsafe { c_api::read_i64_raw(page.chunk(col_idx), row_idx) }),
          Ty::F32 => write!(buf, "{}", unsafe { c_api::read_f32_raw(page.chunk(col_idx), row_idx) }),
          Ty::F64 => write!(buf, "{}", unsafe { c_api::read_f64_raw(page.chunk(col_idx), row_idx) }),
          _ => panic!("not supported")
        }.unwrap();
      }
    }
  }
}
