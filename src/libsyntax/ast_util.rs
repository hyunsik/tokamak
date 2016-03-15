use ast::*;
use ast;
use codemap;
use codemap::Span;
use parse::token;
use print::pprust;
use ptr::P;

use std::cmp;
use std::u32;

pub fn path_name_i(idents: &[Ident]) -> String {
    // FIXME: Bad copies (#2543 -- same for everything else that says "bad")
    idents.iter().map(|i| i.to_string()).collect::<Vec<String>>().join("::")
}

pub fn is_path(e: P<Expr>) -> bool {
    match e.node { ExprKind::Path(..) => true, _ => false }
}


// convert a span and an identifier to the corresponding
// 1-segment path
pub fn ident_to_path(s: Span, identifier: Ident) -> Path {
    ast::Path {
        span: s,
        global: false,
        segments: vec!(
            ast::PathSegment {
                identifier: identifier,
                parameters: ast::PathParameters::AngleBracketed(ast::AngleBracketedParameterData {
                    lifetimes: Vec::new(),
                    types: P::empty(),
                    bindings: P::empty(),
                })
            }
        ),
    }
}

// If path is a single segment ident path, return that ident. Otherwise, return
// None.
pub fn path_to_ident(path: &Path) -> Option<Ident> {
    if path.segments.len() != 1 {
        return None;
    }

    let segment = &path.segments[0];
    if !segment.parameters.is_empty() {
        return None;
    }

    Some(segment.identifier)
}

pub fn ident_to_pat(id: NodeId, s: Span, i: Ident) -> P<Pat> {
    let spanned = codemap::Spanned{ span: s, node: i };
    P(Pat {
        id: id,
        node: PatKind::Ident(BindingMode::ByValue(Mutability::Immutable), spanned, None),
        span: s
    })
}

// ______________________________________________________________________
// Enumerating the IDs which appear in an AST

#[derive(Copy, Clone, RustcEncodable, RustcDecodable, Debug)]
pub struct IdRange {
    pub min: NodeId,
    pub max: NodeId,
}

impl IdRange {
    pub fn max() -> IdRange {
        IdRange {
            min: u32::MAX,
            max: u32::MIN,
        }
    }

    pub fn empty(&self) -> bool {
        self.min >= self.max
    }

    pub fn add(&mut self, id: NodeId) {
        self.min = cmp::min(self.min, id);
        self.max = cmp::max(self.max, id + 1);
    }
}

pub trait IdVisitingOperation {
    fn visit_id(&mut self, node_id: NodeId);
}

/// A visitor that applies its operation to all of the node IDs
/// in a visitable thing.

pub struct IdVisitor<'a, O:'a> {
    pub operation: &'a mut O,
    pub visited_outermost: bool,
}


impl<'a, O: IdVisitingOperation> IdVisitor<'a, O> {
    fn visit_generics_helper(&mut self, generics: &Generics) {
        for type_parameter in generics.ty_params.iter() {
            self.operation.visit_id(type_parameter.id)
        }
        for lifetime in &generics.lifetimes {
            self.operation.visit_id(lifetime.lifetime.id)
        }
    }
}

pub struct IdRangeComputingVisitor {
    pub result: IdRange,
}

impl IdRangeComputingVisitor {
    pub fn new() -> IdRangeComputingVisitor {
        IdRangeComputingVisitor { result: IdRange::max() }
    }

    pub fn result(&self) -> IdRange {
        self.result
    }
}

impl IdVisitingOperation for IdRangeComputingVisitor {
    fn visit_id(&mut self, id: NodeId) {
        self.result.add(id);
    }
}