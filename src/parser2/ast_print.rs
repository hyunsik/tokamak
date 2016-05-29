use itertools::Itertools;

use ast::{self, Path, PathSegment};

pub fn path_to_string(p: &ast::Path) -> String {
  let path_str = p.segments.iter().join("::");

  if p.global {
    format!("::{}", &path_str)
  } else {
    path_str
  }
}