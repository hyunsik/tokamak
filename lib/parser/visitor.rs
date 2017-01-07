// Copyright 2012-2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! AST walker. Each overridden visit method has full control over what
//! happens with its node, it can do its own traversal of the node's children,
//! call `visit::walk_*` to apply the default traversal algorithm, or prevent
//! deeper traversal by doing nothing.
//!
//! Note: it is an important invariant that the default visitor walks the body
//! of a function in "execution order" (more concretely, reverse post-order
//! with respect to the CFG implied by the AST), meaning that if AST node A may
//! execute before AST node B, then A is visited first.  The borrow checker in
//! particular relies on this property.
//!
//! Note: walking an AST before macro expansion is probably a bad idea. For
//! instance, a walker looking for item names in a module will miss all of
//! those that are created by the expansion of a macro.

use abi::Abi;
use ast::*;
use common::codespan::{Span};

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum FnKind<'a> {
  /// fn foo() or extern "Abi" fn foo()
  ItemFn(Ident, Unsafety, Constness, Abi, &'a Visibility),

  /// |x, y| {}
  Closure,
}

/// Each method of the Visitor trait is a hook to be potentially
/// overridden.  Each method's default implementation recursively visits
/// the substructure of the input via the corresponding `walk` method;
/// e.g. the `visit_mod` method by default calls `visit::walk_mod`.
///
/// If you want to ensure that your code handles every variant
/// explicitly, you need to override each method.  (And you also need
/// to monitor future changes to `Visitor` in case a new method with a
/// new default implementation gets introduced.)
pub trait Visitor<'ast>: Sized {
  fn visit_name(&mut self, _span: Span, _name: Name) {
        // Nothing to do.
    }
    fn visit_ident(&mut self, span: Span, ident: Ident) {
        unimplemented!()
    }
    fn visit_mod(&mut self, m: &'ast Module, _s: Span, _n: NodeId) {
        unimplemented!()
    }
    fn visit_foreign_item(&mut self, i: &'ast ForeignItem) {
        unimplemented!()
    }
    fn visit_item(&mut self, i: &'ast Item) {
        unimplemented!()
    }
    fn visit_local(&mut self, l: &'ast Local) {
        unimplemented!()
    }
    fn visit_block(&mut self, b: &'ast Block) {
        unimplemented!()
    }
    fn visit_stmt(&mut self, s: &'ast Stmt) {
        unimplemented!()
     }
    fn visit_arm(&mut self, a: &'ast Arm) {
        unimplemented!()
    }
    fn visit_pat(&mut self, p: &'ast Pat) {
        unimplemented!()
    }
    fn visit_expr(&mut self, ex: &'ast Expr) {
        unimplemented!()
    }
    fn visit_expr_post(&mut self, _ex: &'ast Expr) {
    }
    fn visit_ty(&mut self, t: &'ast Ty) {
        unimplemented!()
    }
    fn visit_fn(&mut self, fk: FnKind<'ast>, fd: &'ast FnDecl, s: Span, _: NodeId) {
        unimplemented!()
    }
    fn visit_path(&mut self, path: &'ast Path, _id: NodeId) {
        unimplemented!()
    }
    fn visit_path_list_item(&mut self, prefix: &'ast Path, item: &'ast PathListItem) {
        unimplemented!()
    }
    fn visit_path_segment(&mut self, path_span: Span, path_segment: &'ast PathSegment) {
        unimplemented!()
    }
    fn visit_attribute(&mut self, _attr: &'ast Attribute) {}
    fn visit_vis(&mut self, vis: &'ast Visibility) {
        unimplemented!()
    }
    fn visit_fn_ret_ty(&mut self, ret_ty: &'ast FunctionRetTy) {
        unimplemented!()
    }
}