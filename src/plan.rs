//!
//! Plan Representation for Tajo Kernel
//!

use common::schema::{Column, Schema};
use url::Url;

trait Eval {
  fn eval();
}

enum Fragment {
  File {uri: Url, offset: u64, length: u64}
}

enum PlanNode {
  SeqScan {project_columns: Vec<Column>, filter_cond: Vec<Box<Eval>>},
  Project {exprs: Vec<Box<Eval>>, child: Box<PlanNode>}
}