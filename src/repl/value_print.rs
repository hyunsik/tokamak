use std::fmt::Write;
use std::convert::Into;
use libc::c_void;
use llvm::{self, JitCompiler};
use llvm::builder::Builder;
use llvm::value::Function;

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

encode_value!(encode_bool, bool);
encode_value!(encode_i8, i8);
encode_value!(encode_i16, i16);
encode_value!(encode_i32, i32);
encode_value!(encode_i64, i64);
encode_value!(encode_f32, f32);
encode_value!(encode_f64, f64);

pub struct ValuePrint<'a> {
  jit: &'a JitCompiler,
  rstr_ty: llvm::Ty,
}

macro_rules! trans(
  ($func:expr) => (
    unsafe { ::std::mem::transmute($func) };
  );
);

impl<'a> ValuePrint<'a> {
  pub fn new(jit: &JitCompiler) -> ValuePrint {
    let vp = ValuePrint {
      jit: jit,
      rstr_ty: jit.get_ty("struct.RustStringRef")
                  .expect("struct.RustStringRef does not exist.")
                  .pointer_ty(),
    };

    vp.register_fn("encode_bool", jit.get_bool_ty(), trans!(encode_bool));
    vp.register_fn("encode_i8", jit.get_i8_ty(), trans!(encode_i8));
    vp.register_fn("encode_i16", jit.get_i16_ty(), trans!(encode_i16));
    vp.register_fn("encode_i32", jit.get_i32_ty(), trans!(encode_i32));
    vp.register_fn("encode_i64", jit.get_i64_ty(), trans!(encode_i64));
    vp.register_fn("encode_f32", jit.get_f32_ty(), trans!(encode_f32));
    vp.register_fn("encode_f64", jit.get_f64_ty(), trans!(encode_f64));

    vp
  }

  fn register_fn(&self, name: &str, ty: &llvm::Ty, fn_ptr: *const c_void) {
    let jit = self.jit;
    let func = jit.create_func_prototype(name, jit.get_void_ty(), &[&self.rstr_ty, ty], None);
    unsafe { jit.add_global_mapping(&func, fn_ptr) };
  }

  // void print(*RustStringRef, val: <?>)
  fn create_fn_proto(&self, bld: &Builder, val_ty: &Ty) -> Function {
    self.jit.create_func_prototype("print_value",
                                   self.jit.get_void_ty(),
                                   &[&self.rstr_ty, to_llvm_ty(self.jit, val_ty)],
                                   Some(bld))
  }

  fn build(&self, val_fn: &Function, val_ty: &Ty) -> Function {
    let bld = self.jit.new_builder();
    let print_fn = self.create_fn_proto(&bld, val_ty);

    let encode_fn_name = match *val_ty {
      Ty::Bool => "encode_bool",
      Ty::I8 => "encode_i8",
      Ty::I16 => "encode_i16",
      Ty::I32 => "encode_i32",
      Ty::I64 => "encode_i64",
      Ty::F32 => "encode_f32",
      Ty::F64 => "encode_f64",
      _ => panic!("not supported type"),
    };

    let encode_fn = self.jit
                        .get_func(encode_fn_name)
                        .expect(&format!("function {} does not exist", encode_fn_name));

    bld.create_call(&encode_fn,
                    &[&print_fn.arg(0).into(), // *RustStringRef
                      &bld.create_call(val_fn, &[]) /* value */]);

    bld.create_ret_void();

    print_fn
  }

  pub fn print(&self, val_fn: &Function, ty: &Ty, buf: &mut String) {
    let print_fn = &self.build(val_fn, ty);

    let fn_ptr: fn(&mut String) = trans!(self.jit
                                             .get_func_ptr(print_fn)
                                             .expect("Value Print function does not exist."));

    //self.jit.dump();
    //self.jit.verify().ok().expect("Module verification failed...");
    fn_ptr(buf);
    self.jit.delete_func(print_fn);
  }
}
