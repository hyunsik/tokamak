use std::fmt;

use ast;
use attr::ThinAttributes;
use codemap::{Span, Spanned};
use ptr::P;
use token::{self, InternedString};

pub type NodeId = u32;

/// Node id used to represent the root of the package.
pub const PKG_NODE_ID: NodeId = 0;

/// When parsing and doing expansions, we initially give all AST nodes this AST
/// node value. Then later, in the renumber pass, we renumber them to have
/// small, positive ids.
pub const DUMMY_NODE_ID: NodeId = !0;

/// A name is a part of an identifier, representing a string or gensym. It's
/// the result of interning.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(pub u32);

impl Name {
  pub fn as_str(self) -> token::InternedString {
    token::InternedString::new_from_name(self)
  }
}

impl fmt::Debug for Name {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}({})", self, self.0)
  }
}

impl fmt::Display for Name {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    fmt::Display::fmt(&self.as_str(), f)
  }
}

/// A SyntaxContext represents a chain of macro-expandings
/// and renamings. Each macro expansion corresponds to
/// a fresh u32. This u32 is a reference to a table stored
/// in thread-local storage.
/// The special value EMPTY_CTXT is used to indicate an empty
/// syntax context.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct SyntaxContext(pub u32);

/// An identifier contains a Name (index into the interner
/// table) and a SyntaxContext to track renaming and
/// macro expansion per Flatt et al., "Macros That Work Together"
#[derive(Clone, Copy, Eq)]
pub struct Ident {
  pub name: Name,
  pub ctxt: SyntaxContext
}

pub const EMPTY_CTXT : SyntaxContext = SyntaxContext(0);

impl Ident {
  pub fn new(name: Name, ctxt: SyntaxContext) -> Ident {
    Ident {name: name, ctxt: ctxt}
  }
  pub fn with_empty_ctxt(name: Name) -> Ident {
    Ident {name: name, ctxt: EMPTY_CTXT}
  }
}

impl PartialEq for Ident {
  fn eq(&self, other: &Ident) -> bool {
    if self.ctxt != other.ctxt {
      // There's no one true way to compare Idents. They can be compared
      // non-hygienically `id1.name == id2.name`, hygienically
      // `mtwt::resolve(id1) == mtwt::resolve(id2)`, or even member-wise
      // `(id1.name, id1.ctxt) == (id2.name, id2.ctxt)` depending on the situation.
      // Ideally, PartialEq should not be implemented for Ident at all, but that
      // would be too impractical, because many larger structures (Token, in particular)
      // including Idents as their parts derive PartialEq and use it for non-hygienic
      // comparisons. That's why PartialEq is implemented and defaults to non-hygienic
      // comparison. Hash is implemented too and is consistent with PartialEq, i.e. only
      // the name of Ident is hashed. Still try to avoid comparing idents in your code
      // (especially as keys in hash maps), use one of the three methods listed above
      // explicitly.
      //
      // If you see this panic, then some idents from different contexts were compared
      // non-hygienically. It's likely a bug. Use one of the three comparison methods
      // listed above explicitly.

      panic!("idents with different contexts are compared with operator `==`: \
                {:?}, {:?}.", self, other);
    }

    self.name == other.name
  }
}


impl fmt::Debug for Ident {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}#{}", self.name, self.ctxt.0)
  }
}

impl fmt::Display for Ident {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    fmt::Display::fmt(&self.name, f)
  }
}

pub struct Package {
  pub module: Module,
  pub span: Span
}

pub struct Module {
  pub span: Span,
  pub items: Vec<Box<Item>>
}

pub enum Visibility {
  Public,
  Inherited,
}

pub struct Item {
  pub node: ItemKind,
  pub span: Span
}

pub enum ItemKind {
  Import,

  Const,
  Static,

  Ty,
  Enum,
  Struct,

  Fn,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Expr {
  pub id: NodeId,
  pub node: ExprKind,
  pub span: Span,
  pub attrs: ThinAttributes
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum ExprKind {
  Call,
  /// A unary operation (For example: `!x`, `*x`)
  Unary(UnOp, P<Expr>),
  Binary(BinOp, P<Expr>, P<Expr>),
  Literal,
  Cast,
  If,
  While,
  Loop,
  ForLoop,
  Match,
  Block,
  Assign,
  Path,
  Paren
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub enum UnOp {
  /// The `!` operator for logical inversion
  Not,
  /// The `-` operator for negation
  Neg,
}

impl UnOp {
  pub fn to_string(op: UnOp) -> &'static str {
    match op {
      UnOp::Not => "!",
      UnOp::Neg => "-",
    }
  }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub enum BinOpKind {
  /// The `+` operator (addition)
  Add,
  /// The `-` operator (subtraction)
  Sub,
  /// The `*` operator (multiplication)
  Mul,
  /// The `/` operator (division)
  Div,
  /// The `%` operator (modulus)
  Rem,
  /// The `&&` operator (logical and)
  And,
  /// The `||` operator (logical or)
  Or,
  /// The `^` operator (bitwise xor)
  BitXor,
  /// The `&` operator (bitwise and)
  BitAnd,
  /// The `|` operator (bitwise or)
  BitOr,
  /// The `<<` operator (shift left)
  Shl,
  /// The `>>` operator (shift right)
  Shr,
  /// The `==` operator (equality)
  Eq,
  /// The `<` operator (less than)
  Lt,
  /// The `<=` operator (less than or equal to)
  Le,
  /// The `!=` operator (not equal to)
  Ne,
  /// The `>=` operator (greater than or equal to)
  Ge,
  /// The `>` operator (greater than)
  Gt,
}

pub type BinOp = Spanned<BinOpKind>;

/// Meta-data associated with an item
pub type Attribute = Spanned<Attribute_>;

#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub struct AttrId(pub usize);

/// Doc-comments are promoted to attributes that have is_sugared_doc = true
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Attribute_ {
  pub id: AttrId,
  pub value: P<MetaItem>,
}

pub type MetaItem = Spanned<MetaItemKind>;

#[derive(Clone, Eq, Hash, Debug)]
pub enum MetaItemKind {
  Word(InternedString),
  List(InternedString, Vec<P<MetaItem>>),
  //NameValue(InternedString, Lit),
}

// can't be derived because the MetaItemKind::List requires an unordered comparison
impl PartialEq for MetaItemKind {
  fn eq(&self, other: &MetaItemKind) -> bool {
    use self::MetaItemKind::*;
    match *self {
      Word(ref ns) => match *other {
        Word(ref no) => (*ns) == (*no),
        _ => false
      },
      List(ref ns, ref miss) => match *other {
        List(ref no, ref miso) => {
          ns == no &&
          miss.iter().all(|mi| miso.iter().any(|x| x.node == mi.node))
        }
        _ => false
      }
    }
  }
}