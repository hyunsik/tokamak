extern crate gcc;

use std::process::Command;

static IR_TARGET_PATH: &'static str = "target/ir";

// Prepare a directory
fn prepare_dir(path: &str) {
  assert!(
    Command::new("mkdir")
      .args(&["-p", path])
      .status()
      .unwrap()
      .success()
  );
}

// Emit C++ source file into LLVM IR and LLVM Bitcode
fn emit_llvm_ir(src_fname: &str, ir_fname: &str, bc_fname: &str) {
  let ir_path  = &format!("{}/{}", IR_TARGET_PATH, ir_fname);
  let bc_path  = &format!("{}/{}", IR_TARGET_PATH, bc_fname);

  assert!(
  	Command::new("clang++")
  		.args(&[
        src_fname,
        "-std=c++11",
        "-S", "-emit-llvm",
        "-O2",
        "-o", ir_path
      ])
    	.status()
    	.unwrap()
    	.success()
 	);
  assert!(
  	Command::new("llvm-as")
  		.args(&[ir_path, &format!("-o={}", bc_path)])
    	.status()
    	.unwrap()
    	.success()
 	);
}

// Link multiple bc files into a single bc file.
fn link_llvm_bc(src_bc: &[&str], target_bc: &str) {
  let mut args = src_bc.iter().map(|name| format!("{}/{}", IR_TARGET_PATH, name)).collect::<Vec<String>>();
  args.push(format!("-o={}/{}", IR_TARGET_PATH, target_bc));

  assert!(
  	Command::new("llvm-link")
  		.args(&args)
    	.status()
    	.unwrap()
    	.success()
 	);
}

fn main() {

  prepare_dir(IR_TARGET_PATH);

  emit_llvm_ir("page_ir.cc", "page_ir.ll", "page_ir.bc");
  emit_llvm_ir("common_ir.cc", "common_ir.ll", "common_ir.bc");

  link_llvm_bc(&["page_ir.bc", "common_ir.bc"], "common.bc");

  gcc::Config::new()
    .cpp(true)
    .file("page_ir.cc")
    .compile("libpage.a");
}
