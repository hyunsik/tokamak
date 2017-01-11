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
use common::codespan::Span;
use codemap::Spanned;

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum FnKind<'a> {
    /// fn foo() or extern "Abi" fn foo()
    ItemFn(Ident, &'a Generics, Unsafety, Spanned<Constness>, Abi, &'a Visibility, &'a Block),

    /// |x, y| {}
    Closure(&'a Expr),
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
        walk_foreign_item(self, i)
    }
    fn visit_item(&mut self, i: &'ast Item) {
        walk_item(self, i)
    }
    fn visit_local(&mut self, l: &'ast Local) {
        walk_local(self, l)
    }
    fn visit_block(&mut self, b: &'ast Block) {
        walk_block(self, b)
    }
    fn visit_stmt(&mut self, s: &'ast Stmt) {
        walk_stmt(self, s)
     }
    fn visit_arm(&mut self, a: &'ast Arm) {
        walk_arm(self, a)
    }
    fn visit_pat(&mut self, p: &'ast Pat) {
        walk_pat(self, p)
    }
    fn visit_expr(&mut self, ex: &'ast Expr) {
        walk_expr(self, ex)
    }
    fn visit_expr_post(&mut self, _ex: &'ast Expr) { }
    fn visit_ty(&mut self, t: &'ast Ty) {
        walk_ty(self, t)
    }
    fn visit_generics(&mut self, g: &'ast Generics) {
        walk_generics(self, g)
    }
    fn visit_fn(&mut self, fk: FnKind<'ast>, fd: &'ast FnDecl, s: Span, _: NodeId) {
        walk_fn(self, fk, fd, s)
    }
    fn visit_path(&mut self, path: &'ast Path, _id: NodeId) {
        walk_path(self, path)
    }
    fn visit_path_list_item(&mut self, prefix: &'ast Path, item: &'ast PathListItem) {
        walk_path_list_item(self, prefix, item)
    }
    fn visit_path_segment(&mut self, path_span: Span, path_segment: &'ast PathSegment) {
        walk_path_segment(self, path_span, path_segment)
    }
    fn visit_attribute(&mut self, _attr: &'ast Attribute) {}
    fn visit_vis(&mut self, vis: &'ast Visibility) {
        walk_vis(self, vis)
    }
    fn visit_fn_ret_ty(&mut self, ret_ty: &'ast FunctionRetTy) {
        walk_fn_ret_ty(self, ret_ty)
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

pub fn walk_opt_ident<'a, V: Visitor<'a>>(visitor: &mut V, span: Span, opt_ident: Option<Ident>) {
    if let Some(ident) = opt_ident {
        visitor.visit_ident(span, ident);
    }
}

pub fn walk_opt_sp_ident<'a, V: Visitor<'a>>(visitor: &mut V,
                                             opt_sp_ident: &Option<Spanned<Ident>>) {
    if let Some(ref sp_ident) = *opt_sp_ident {
        visitor.visit_ident(sp_ident.span, sp_ident.node);
    }
}

pub fn walk_ident<'a, V: Visitor<'a>>(visitor: &mut V, span: Span, ident: Ident) {
    visitor.visit_name(span, ident.name);
}

pub fn walk_package<'a, V: Visitor<'a>>(visitor: &mut V, pkg: &'a Package) {
    visitor.visit_mod(&pkg.module, pkg.span, PACKAGE_NODE_ID);
    walk_list!(visitor, visit_attribute, &pkg.attrs);
}

pub fn walk_mod<'a, V: Visitor<'a>>(visitor: &mut V, module: &'a Module) {
    walk_list!(visitor, visit_item, &module.items);
}

pub fn walk_pat<'a, V: Visitor<'a>>(visitor: &mut V, pattern: &'a Pat) {
    match pattern.node {
        PatKind::TupleStruct(ref path, ref children, _) => {
            visitor.visit_path(path, pattern.id);
            walk_list!(visitor, visit_pat, children);
        }
        PatKind::Path(ref opt_qself, ref path) => {
            if let Some(ref qself) = *opt_qself {
                visitor.visit_ty(&qself.ty);
            }
            visitor.visit_path(path, pattern.id)
        }
        PatKind::Struct(ref path, ref fields, _) => {
            visitor.visit_path(path, pattern.id);
            for field in fields {
                visitor.visit_ident(field.span, field.node.ident);
                visitor.visit_pat(&field.node.pat)
            }
        }
        PatKind::Tuple(ref tuple_elements, _) => {
            walk_list!(visitor, visit_pat, tuple_elements);
        }
        PatKind::Ident(_, ref pth1, ref optional_subpattern) => {
            visitor.visit_ident(pth1.span, pth1.node);
            walk_list!(visitor, visit_pat, optional_subpattern);
        }
        PatKind::Lit(ref expression) => visitor.visit_expr(expression),
        PatKind::Range(ref lower_bound, ref upper_bound) => {
            visitor.visit_expr(lower_bound);
            visitor.visit_expr(upper_bound)
        }
        PatKind::Wild => (),
        PatKind::Slice(ref prepatterns, ref slice_pattern, ref postpatterns) => {
            walk_list!(visitor, visit_pat, prepatterns);
            walk_list!(visitor, visit_pat, slice_pattern);
            walk_list!(visitor, visit_pat, postpatterns);
        }
    }
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
    match typ.node {
        TyKind::Slice(ref ty) | TyKind::Paren(ref ty) => {
            visitor.visit_ty(ty)
        }
        TyKind::Tup(ref tuple_element_types) => {
            walk_list!(visitor, visit_ty, tuple_element_types);
        }
        TyKind::Path(ref path) => {
            visitor.visit_path(path, typ.id);
        }
        TyKind::Array(ref ty, ref expression) => {
            visitor.visit_ty(ty);
            visitor.visit_expr(expression)
        }
        TyKind::Infer => {}
    }
}

pub fn walk_path<'a, V: Visitor<'a>>(visitor: &mut V, path: &'a Path) {
    for segment in &path.segments {
        visitor.visit_path_segment(path.span, segment);
    }
}

pub fn walk_path_list_item<'a, V: Visitor<'a>>(visitor: &mut V,
                                               _prefix: &Path,
                                               item: &'a PathListItem) {
    visitor.visit_ident(item.span, item.node.name);
    walk_opt_ident(visitor, item.span, item.node.rename);
}

pub fn walk_path_segment<'a, V: Visitor<'a>>(visitor: &mut V,
                                             path_span: Span,
                                             segment: &'a PathSegment) {
    visitor.visit_ident(path_span, segment.identifier);
}

pub fn walk_generics<'a, V: Visitor<'a>>(visitor: &mut V, generics: &'a Generics) {
    for param in &generics.ty_params {
        visitor.visit_ident(param.span, param.ident);
        walk_list!(visitor, visit_ty, &param.default);
        walk_list!(visitor, visit_attribute, &*param.attrs);
    }
}

pub fn walk_fn_ret_ty<'a, V: Visitor<'a>>(visitor: &mut V, ret_ty: &'a FunctionRetTy) {
    if let FunctionRetTy::Ty(ref output_ty) = *ret_ty {
        visitor.visit_ty(output_ty)
    }
}

pub fn walk_fn_decl<'a, V: Visitor<'a>>(visitor: &mut V, function_declaration: &'a FnDecl) {
    for argument in &function_declaration.inputs {
        visitor.visit_pat(&argument.pat);
        visitor.visit_ty(&argument.ty)
    }
    visitor.visit_fn_ret_ty(&function_declaration.output)
}

pub fn walk_fn<'a, V>(visitor: &mut V, kind: FnKind<'a>, declaration: &'a FnDecl, _span: Span)
    where V: Visitor<'a>,
{
    match kind {
        FnKind::ItemFn(_, generics, _, _, _, _, body) => {
            visitor.visit_generics(generics);
            walk_fn_decl(visitor, declaration);
            visitor.visit_block(body);
        }
        FnKind::Closure(body) => {
            walk_fn_decl(visitor, declaration);
            visitor.visit_expr(body);
        }
    }
}

pub fn walk_local<'a, V: Visitor<'a>>(visitor: &mut V, local: &'a Local) {
    for attr in local.attrs.iter() {
        visitor.visit_attribute(attr);
    }
    visitor.visit_pat(&local.pat);
    walk_list!(visitor, visit_ty, &local.ty);
    walk_list!(visitor, visit_expr, &local.init);
}

pub fn walk_item<'a, V: Visitor<'a>>(visitor: &mut V, item: &'a Item) {
    visitor.visit_vis(&item.vis);
    visitor.visit_ident(item.span, item.ident);
    match item.node {
        ItemKind::Import(ref vp) => {
            match vp.node {
                ViewPathSimple(ident, ref path) => {
                    visitor.visit_ident(vp.span, ident);
                    visitor.visit_path(path, item.id);
                }
                ViewPathGlob(ref path) => {
                    visitor.visit_path(path, item.id);
                }
                ViewPathList(ref prefix, ref list) => {
                    visitor.visit_path(prefix, item.id);
                    for item in list {
                        visitor.visit_path_list_item(prefix, item)
                    }
                }
            }
        }
        ItemKind::ForeignMod(ref foreign_module) => {
            walk_list!(visitor, visit_foreign_item, &foreign_module.items);
        }
        ItemKind::Static(ref typ, _, ref expr) |
        ItemKind::Const(ref typ, ref expr) => {
            visitor.visit_ty(typ);
            visitor.visit_expr(expr);
        }
        ItemKind::Fn(ref declaration, unsafety, constness, abi, ref generics, ref body) => {
            visitor.visit_fn(FnKind::ItemFn(item.ident, generics, unsafety,
                                            constness, abi, &item.vis, body),
                             declaration,
                             item.span,
                             item.id)
        }
        ItemKind::Ty(ref typ, ref type_parameters) => {
            visitor.visit_ty(typ);
            visitor.visit_generics(type_parameters)
        }
        ItemKind::Enum => {
        }
        ItemKind::Struct => {
        }
    }
    walk_list!(visitor, visit_attribute, &item.attrs);
}

pub fn walk_block<'a, V: Visitor<'a>>(visitor: &mut V, block: &'a Block) {
    walk_list!(visitor, visit_stmt, &block.stmts);
}

pub fn walk_stmt<'a, V: Visitor<'a>>(visitor: &mut V, statement: &'a Stmt) {
    match statement.node {
        StmtKind::Local(ref local) => visitor.visit_local(local),
        StmtKind::Item(ref item) => visitor.visit_item(item),
        StmtKind::Expr(ref expression) | StmtKind::Semi(ref expression) => {
            visitor.visit_expr(expression)
        }
    }
}

pub fn walk_expr<'a, V: Visitor<'a>>(visitor: &mut V, expression: &'a Expr) {
    for attr in expression.attrs.iter() {
        visitor.visit_attribute(attr);
    }

    match expression.node {
        ExprKind::InPlace(ref place, ref subexpression) => {
            visitor.visit_expr(place);
            visitor.visit_expr(subexpression)
        }
        ExprKind::Vec(ref subexpressions) => {
            walk_list!(visitor, visit_expr, subexpressions);
        }
        ExprKind::Repeat(ref element, ref count) => {
            visitor.visit_expr(element);
            visitor.visit_expr(count)
        }
        ExprKind::Struct(ref path, ref fields, ref optional_base) => {
            visitor.visit_path(path, expression.id);
            for field in fields {
                visitor.visit_ident(field.ident.span, field.ident.node);
                visitor.visit_expr(&field.expr)
            }
            walk_list!(visitor, visit_expr, optional_base);
        }
        ExprKind::Tup(ref subexpressions) => {
            walk_list!(visitor, visit_expr, subexpressions);
        }
        ExprKind::Call(ref callee_expression, ref arguments) => {
            visitor.visit_expr(callee_expression);
            walk_list!(visitor, visit_expr, arguments);
        }
        ExprKind::MethodCall(ref ident, ref types, ref arguments) => {
            visitor.visit_ident(ident.span, ident.node);
            walk_list!(visitor, visit_ty, types);
            walk_list!(visitor, visit_expr, arguments);
        }
        ExprKind::Binary(_, ref left_expression, ref right_expression) => {
            visitor.visit_expr(left_expression);
            visitor.visit_expr(right_expression)
        }
        ExprKind::Unary(_, ref subexpression) => {
            visitor.visit_expr(subexpression)
        }
        ExprKind::Lit(_) => {}
        ExprKind::Cast(ref subexpression, ref typ) | ExprKind::Type(ref subexpression, ref typ) => {
            visitor.visit_expr(subexpression);
            visitor.visit_ty(typ)
        }
        ExprKind::If(ref head_expression, ref if_block, ref optional_else) => {
            visitor.visit_expr(head_expression);
            visitor.visit_block(if_block);
            walk_list!(visitor, visit_expr, optional_else);
        }
        ExprKind::While(ref subexpression, ref block, ref opt_sp_ident) => {
            visitor.visit_expr(subexpression);
            visitor.visit_block(block);
            walk_opt_sp_ident(visitor, opt_sp_ident);
        }
        ExprKind::IfLet(ref pattern, ref subexpression, ref if_block, ref optional_else) => {
            visitor.visit_pat(pattern);
            visitor.visit_expr(subexpression);
            visitor.visit_block(if_block);
            walk_list!(visitor, visit_expr, optional_else);
        }
        ExprKind::WhileLet(ref pattern, ref subexpression, ref block, ref opt_sp_ident) => {
            visitor.visit_pat(pattern);
            visitor.visit_expr(subexpression);
            visitor.visit_block(block);
            walk_opt_sp_ident(visitor, opt_sp_ident);
        }
        ExprKind::ForLoop(ref pattern, ref subexpression, ref block, ref opt_sp_ident) => {
            visitor.visit_pat(pattern);
            visitor.visit_expr(subexpression);
            visitor.visit_block(block);
            walk_opt_sp_ident(visitor, opt_sp_ident);
        }
        ExprKind::Loop(ref block, ref opt_sp_ident) => {
            visitor.visit_block(block);
            walk_opt_sp_ident(visitor, opt_sp_ident);
        }
        ExprKind::Match(ref subexpression, ref arms) => {
            visitor.visit_expr(subexpression);
            walk_list!(visitor, visit_arm, arms);
        }
        ExprKind::Closure(_, ref function_declaration, ref body, _decl_span) => {
            visitor.visit_fn(FnKind::Closure(body),
                             function_declaration,
                             expression.span,
                             expression.id)
        }
        ExprKind::Block(ref block) => visitor.visit_block(block),
        ExprKind::Assign(ref left_hand_expression, ref right_hand_expression) => {
            visitor.visit_expr(left_hand_expression);
            visitor.visit_expr(right_hand_expression);
        }
        ExprKind::AssignOp(_, ref left_expression, ref right_expression) => {
            visitor.visit_expr(left_expression);
            visitor.visit_expr(right_expression);
        }
        ExprKind::Field(ref subexpression, ref ident) => {
            visitor.visit_expr(subexpression);
            visitor.visit_ident(ident.span, ident.node);
        }
        ExprKind::TupField(ref subexpression, _) => {
            visitor.visit_expr(subexpression);
        }
        ExprKind::Index(ref main_expression, ref index_expression) => {
            visitor.visit_expr(main_expression);
            visitor.visit_expr(index_expression)
        }
        ExprKind::Range(ref start, ref end, _) => {
            walk_list!(visitor, visit_expr, start);
            walk_list!(visitor, visit_expr, end);
        }
        ExprKind::Path(ref maybe_qself, ref path) => {
            if let Some(ref qself) = *maybe_qself {
                visitor.visit_ty(&qself.ty);
            }
            visitor.visit_path(path, expression.id)
        }
        ExprKind::Break(ref opt_sp_ident) => {
            walk_opt_sp_ident(visitor, opt_sp_ident);
        }
        ExprKind::Continue(ref opt_sp_ident) => {
            walk_opt_sp_ident(visitor, opt_sp_ident);
        }
        ExprKind::Ret(ref optional_expression) => {
            walk_list!(visitor, visit_expr, optional_expression);
        }
        ExprKind::Paren(ref subexpression) => {
            visitor.visit_expr(subexpression)
        }
    }

    visitor.visit_expr_post(expression)
}

pub fn walk_arm<'a, V: Visitor<'a>>(visitor: &mut V, arm: &'a Arm) {
    walk_list!(visitor, visit_pat, &arm.pats);
    walk_list!(visitor, visit_expr, &arm.guard);
    visitor.visit_expr(&arm.body);
    walk_list!(visitor, visit_attribute, &arm.attrs);
}

pub fn walk_vis<'a, V: Visitor<'a>>(visitor: &mut V, vis: &'a Visibility) {
    if let Visibility::Restricted { ref path, id } = *vis {
        visitor.visit_path(path, id);
    }
}