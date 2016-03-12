use ast;
use ast::{AttrId, Attribute, Attribute_, MetaItem, MetaItemKind};
use ast::{Stmt, StmtKind, DeclKind};
use ast::{Expr, Item, Local, Decl};
use codemap::{Span, Spanned, spanned, dummy_spanned};
use codemap::BytePos;
use errors::Handler;
use parse::lexer::comments::{doc_comment_style, strip_doc_comment_decoration};
use parse::token::InternedString;
use parse::token;
use ptr::P;

use std::cell::{RefCell, Cell};
use std::collections::HashSet;

thread_local! {
    static USED_ATTRS: RefCell<Vec<u64>> = RefCell::new(Vec::new())
}

pub fn mark_used(attr: &Attribute) {
    let AttrId(id) = attr.node.id;
    USED_ATTRS.with(|slot| {
        let idx = (id / 64) as usize;
        let shift = id % 64;
        if slot.borrow().len() <= idx {
            slot.borrow_mut().resize(idx + 1, 0);
        }
        slot.borrow_mut()[idx] |= 1 << shift;
    });
}

pub fn is_used(attr: &Attribute) -> bool {
    let AttrId(id) = attr.node.id;
    USED_ATTRS.with(|slot| {
        let idx = (id / 64) as usize;
        let shift = id % 64;
        slot.borrow().get(idx).map(|bits| bits & (1 << shift) != 0)
            .unwrap_or(false)
    })
}

pub trait AttrMetaMethods {
    fn check_name(&self, name: &str) -> bool {
        name == &self.name()[..]
    }

    /// Retrieve the name of the meta item, e.g. `foo` in `#[foo]`,
    /// `#[foo="bar"]` and `#[foo(bar)]`
    fn name(&self) -> InternedString;

    /// Gets the string value if self is a MetaItemKind::NameValue variant
    /// containing a string, otherwise None.
    fn value_str(&self) -> Option<InternedString>;
    /// Gets a list of inner meta items from a list MetaItem type.
    fn meta_item_list(&self) -> Option<&[P<MetaItem>]>;

    fn span(&self) -> Span;
}

impl AttrMetaMethods for Attribute {
    fn check_name(&self, name: &str) -> bool {
        let matches = name == &self.name()[..];
        if matches {
            mark_used(self);
        }
        matches
    }
    fn name(&self) -> InternedString { self.meta().name() }
    fn value_str(&self) -> Option<InternedString> {
        self.meta().value_str()
    }
    fn meta_item_list(&self) -> Option<&[P<MetaItem>]> {
        self.node.value.meta_item_list()
    }
    fn span(&self) -> Span { self.meta().span }
}

impl AttrMetaMethods for MetaItem {
    fn name(&self) -> InternedString {
        match self.node {
            MetaItemKind::Word(ref n) => (*n).clone(),
            MetaItemKind::NameValue(ref n, _) => (*n).clone(),
            MetaItemKind::List(ref n, _) => (*n).clone(),
        }
    }

    fn value_str(&self) -> Option<InternedString> {
        match self.node {
            MetaItemKind::NameValue(_, ref v) => {
                match v.node {
                    ast::LitKind::Str(ref s, _) => Some((*s).clone()),
                    _ => None,
                }
            },
            _ => None
        }
    }

    fn meta_item_list(&self) -> Option<&[P<MetaItem>]> {
        match self.node {
            MetaItemKind::List(_, ref l) => Some(&l[..]),
            _ => None
        }
    }
    fn span(&self) -> Span { self.span }
}

// Annoying, but required to get test_cfg to work
impl AttrMetaMethods for P<MetaItem> {
    fn name(&self) -> InternedString { (**self).name() }
    fn value_str(&self) -> Option<InternedString> { (**self).value_str() }
    fn meta_item_list(&self) -> Option<&[P<MetaItem>]> {
        (**self).meta_item_list()
    }
    fn span(&self) -> Span { (**self).span() }
}

pub trait AttributeMethods {
    fn meta(&self) -> &MetaItem;
    fn with_desugared_doc<T, F>(&self, f: F) -> T where
        F: FnOnce(&Attribute) -> T;
}

impl AttributeMethods for Attribute {
    /// Extract the MetaItem from inside this Attribute.
    fn meta(&self) -> &MetaItem {
        &self.node.value
    }

    /// Convert self to a normal #[doc="foo"] comment, if it is a
    /// comment like `///` or `/** */`. (Returns self unchanged for
    /// non-sugared doc attributes.)
    fn with_desugared_doc<T, F>(&self, f: F) -> T where
        F: FnOnce(&Attribute) -> T,
    {
        if self.node.is_sugared_doc {
            let comment = self.value_str().unwrap();
            let meta = mk_name_value_item_str(
                InternedString::new("doc"),
                token::intern_and_get_ident(&strip_doc_comment_decoration(
                        &comment)));
            if self.node.style == ast::AttrStyle::Outer {
                f(&mk_attr_outer(self.node.id, meta))
            } else {
                f(&mk_attr_inner(self.node.id, meta))
            }
        } else {
            f(self)
        }
    }
}

/* Constructors */

pub fn mk_name_value_item_str(name: InternedString, value: InternedString)
                              -> P<MetaItem> {
    let value_lit = dummy_spanned(ast::LitKind::Str(value, ast::StrStyle::Cooked));
    mk_name_value_item(name, value_lit)
}

pub fn mk_name_value_item(name: InternedString, value: ast::Lit)
                          -> P<MetaItem> {
    P(dummy_spanned(MetaItemKind::NameValue(name, value)))
}

pub fn mk_list_item(name: InternedString, items: Vec<P<MetaItem>>) -> P<MetaItem> {
    P(dummy_spanned(MetaItemKind::List(name, items)))
}

pub fn mk_word_item(name: InternedString) -> P<MetaItem> {
    P(dummy_spanned(MetaItemKind::Word(name)))
}

thread_local! { static NEXT_ATTR_ID: Cell<usize> = Cell::new(0) }

pub fn mk_attr_id() -> AttrId {
    let id = NEXT_ATTR_ID.with(|slot| {
        let r = slot.get();
        slot.set(r + 1);
        r
    });
    AttrId(id)
}

/// Returns an inner attribute with the given value.
pub fn mk_attr_inner(id: AttrId, item: P<MetaItem>) -> Attribute {
    dummy_spanned(Attribute_ {
        id: id,
        style: ast::AttrStyle::Inner,
        value: item,
        is_sugared_doc: false,
    })
}

/// Returns an outer attribute with the given value.
pub fn mk_attr_outer(id: AttrId, item: P<MetaItem>) -> Attribute {
    dummy_spanned(Attribute_ {
        id: id,
        style: ast::AttrStyle::Outer,
        value: item,
        is_sugared_doc: false,
    })
}

pub fn mk_sugared_doc_attr(id: AttrId, text: InternedString, lo: BytePos,
                           hi: BytePos)
                           -> Attribute {
    let style = doc_comment_style(&text);
    let lit = spanned(lo, hi, ast::LitKind::Str(text, ast::StrStyle::Cooked));
    let attr = Attribute_ {
        id: id,
        style: style,
        value: P(spanned(lo, hi, MetaItemKind::NameValue(InternedString::new("doc"), lit))),
        is_sugared_doc: true
    };
    spanned(lo, hi, attr)
}


/// A list of attributes, behind a optional box as
/// a space optimization.
pub type ThinAttributes = Option<Box<Vec<Attribute>>>;

pub trait ThinAttributesExt {
    fn map_thin_attrs<F>(self, f: F) -> Self
        where F: FnOnce(Vec<Attribute>) -> Vec<Attribute>;
    fn prepend(mut self, attrs: Self) -> Self;
    fn append(mut self, attrs: Self) -> Self;
    fn update<F>(&mut self, f: F)
        where Self: Sized,
              F: FnOnce(Self) -> Self;
    fn as_attr_slice(&self) -> &[Attribute];
    fn into_attr_vec(self) -> Vec<Attribute>;
}

impl ThinAttributesExt for ThinAttributes {
    fn map_thin_attrs<F>(self, f: F) -> Self
        where F: FnOnce(Vec<Attribute>) -> Vec<Attribute>
    {
        f(self.map(|b| *b).unwrap_or(Vec::new())).into_thin_attrs()
    }

    fn prepend(self, attrs: ThinAttributes) -> Self {
        attrs.map_thin_attrs(|mut attrs| {
            attrs.extend(self.into_attr_vec());
            attrs
        })
    }

    fn append(self, attrs: ThinAttributes) -> Self {
        self.map_thin_attrs(|mut self_| {
            self_.extend(attrs.into_attr_vec());
            self_
        })
    }

    fn update<F>(&mut self, f: F)
        where Self: Sized,
              F: FnOnce(ThinAttributes) -> ThinAttributes
    {
        let self_ = f(self.take());
        *self = self_;
    }

    fn as_attr_slice(&self) -> &[Attribute] {
        match *self {
            Some(ref b) => b,
            None => &[],
        }
    }

    fn into_attr_vec(self) -> Vec<Attribute> {
        match self {
            Some(b) => *b,
            None => Vec::new(),
        }
    }
}


pub trait AttributesExt {
    fn into_thin_attrs(self) -> ThinAttributes;
}

impl AttributesExt for Vec<Attribute> {
    fn into_thin_attrs(self) -> ThinAttributes {
        if self.len() == 0 {
            None
        } else {
            Some(Box::new(self))
        }
    }
}