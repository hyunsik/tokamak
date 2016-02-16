//! Page Printer
//!
//! Convert a page into human-readable string representations according to its
//! encodings and store methods.

use std::io;

use page::{c_api, Page};
use types::Ty;

/// PagePrinter trait
pub trait PagePrinter {
  /// Write the rows into io::Write instace
  fn write(&[&Ty], &Page, &mut io::Write);
}

/// PagePrinter for Columnar and RAW encoding page
pub struct ColumnarPagePrinter;

impl PagePrinter for ColumnarPagePrinter {

  fn write(tys: &[&Ty], page: &Page, buf: &mut io::Write) {
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
          Ty::Bool => {
            if unsafe { c_api::read_i8_raw(page.chunk(col_idx), row_idx) } == 0 {
              write!(buf, "false")
            } else {
              write!(buf, "true")
            }
          },
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
