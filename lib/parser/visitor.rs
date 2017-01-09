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
        walk_ident(self, span, ident);
    }
    fn visit_mod(&mut self, m: &'ast Module, _s: Span, _n: NodeId) {
        walk_mod(self, m)
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
    fn visit_generics(&mut self, g: &'ast Generics) {
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

#[macro_export]
macro_rules! walk_list {
    ($visitor: expr, $method: ident, $list: expr) => {
        for elem in $list {
            $visitor.$method(elem)
        }
    };
    ($visitor: expr, $method: ident, $list: expr, $($extra_args: expr),*) => {
        for elem in $list {
            $visitor.$method(elem, $($extra_args,)*)
        }
    }
}

pub fn walk_package<'a, V: Visitor<'a>>(visitor: &mut V, pkg: &'a Package) {
    visitor.visit_mod(&pkg.module, pkg.span, PACKAGE_NODE_ID);
    walk_list!(visitor, visit_attribute, &pkg.attrs);
}

pub fn walk_mod<'a, V: Visitor<'a>>(visitor: &mut V, module: &'a Module) {
    walk_list!(visitor, visit_item, &module.items);
}

pub fn walk_foreign_item<'a, V: Visitor<'a>>(visitor: &mut V, foreign_item: &'a ForeignItem) {
    visitor.visit_vis(&foreign_item.vis);
    visitor.visit_ident(foreign_item.span, foreign_item.ident);

    match foreign_item.node {
        ForeignItemKind::Fn(ref function_declaration, ref generics) => {
            walk_fn_decl(visitor, function_declaration);
            visitor.visit_generics(generics)
        }
        ForeignItemKind::Static(ref typ, _) => visitor.visit_ty(typ),
    }

    walk_list!(visitor, visit_attribute, &foreign_item.attrs);
}

pub fn walk_ty<'a, V: Visitor<'a>>(visitor: &mut V, typ: &'a Ty) {
    unimplemented!()
}

pub fn walk_ident<'a, V: Visitor<'a>>(visitor: &mut V, span: Span, ident: Ident) {
    visitor.visit_name(span, ident.name);
}

pub fn walk_fn_decl<'a, V: Visitor<'a>>(visitor: &mut V, function_declaration: &'a FnDecl) {
    for argument in &function_declaration.inputs {
        visitor.visit_pat(&argument.pat);
        visitor.visit_ty(&argument.ty)
    }
    visitor.visit_fn_ret_ty(&function_declaration.output)
}