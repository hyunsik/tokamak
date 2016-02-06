use std::fmt::Write;
use std::convert::Into;
use llvm::{self, JitCompiler};
use llvm::builder::Builder;
use llvm::value::{Function, Value};

use common::types::Ty;
use exec::processor::to_llvm_ty;

macro_rules! encode_value(
  ($name:ident, $ty:ty) => (
    #[allow(dead_code)]
    pub extern "C" fn $name(buf: &mut String, val: $ty) {
      write!(buf, "{}", val).unwrap();
    }
  );
);

encode_value!(encode_i8, i8);
encode_value!(encode_i16, i16);
encode_value!(encode_i32, i32);
encode_value!(encode_i64, i64);
encode_value!(encode_f32, f32);
encode_value!(encode_f64, f64);

pub struct ValuePrint<'a> {
  jit: &'a JitCompiler,
  rstr_ty: llvm::Ty
}

impl<'a> ValuePrint<'a> {
  fn new(jit: &JitCompiler) -> ValuePrint {
    ValuePrint {
      jit: jit,
      rstr_ty: jit.get_ty("RustStringRef")
        .expect("RustStringRef does not exist.")
        .pointer_ty()
    }
  }

  // void processor(*RustStringRef)
  fn create_fn_proto(&self, bld: &Builder, val_ty: &Ty) -> Function {
    self.jit.create_func_prototype(
      "processor",
      &self.rstr_ty,
      &[to_llvm_ty(self.jit, val_ty)],
      Some(bld))
  }

  fn build(&self, bld: &Builder, val_ty: &Ty) -> Function {
    let print_fn = self.create_fn_proto(bld, val_ty);

    let encode_fn_name = match *val_ty {
      Ty::I8  => "encode_i8",
      Ty::I16 => "encode_i16",
      Ty::I32 => "encode_i32",
      Ty::I64 => "encode_i64",
      Ty::F32 => "encode_f32",
      Ty::F64 => "encode_f64",
    };

    let encode_fn = self.jit.get_func(encode_fn_name).unwrap();
    bld.create_call(&encode_fn, &[&print_fn.arg(0).into()]);

    print_fn
  }

  fn print(&self, bld: &Builder, val_fn: &Value, ty: &Ty, buf: &mut String) {
    let print_fn = &self.build(bld, ty);
    let fn_ptr: fn(&mut String) = unsafe {
      ::std::mem::transmute(
        self.jit.get_func_ptr(print_fn)
          .expect("Value Print function does not exist.")
      )
    };
    fn_ptr(buf);
  }
}