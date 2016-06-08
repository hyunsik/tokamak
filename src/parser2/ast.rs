use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use ast_print;
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
  pub const fn with_empty_ctxt(name: Name) -> Ident {
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

impl Hash for Ident {
  fn hash<H: Hasher>(&self, state: &mut H) {
    self.name.hash(state)
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

/// A "Path" is essentially Rust's notion of a name; for instance:
/// std::cmp::PartialEq  .  It's represented as a sequence of identifiers,
/// along with a bunch of supporting information.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Path {
  pub span: Span,
  /// A `::foo` path, is relative to the crate root rather than current
  /// module (like paths in an import).
  pub global: bool,
  /// The segments in the path: the things separated by `::`.
  pub segments: Vec<PathSegment>,
}

impl fmt::Debug for Path {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "path({})", ast_print::path_to_string(self))
  }
}

impl fmt::Display for Path {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", ast_print::path_to_string(self))
  }
}

/// A segment of a path: an identifier, an optional lifetime, and a set of
/// types.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct PathSegment {
  /// The identifier portion of this path segment.
  pub identifier: Ident
}

impl fmt::Debug for PathSegment {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "PathSegment({})", self.identifier.name)
  }
}

impl fmt::Display for PathSegment {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "PathSegment({})", self.identifier.name)
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
pub struct Ty {
  pub id: NodeId,
  pub node: TyKind,
  pub span: Span,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
/// The different kinds of types recognized by the compiler
pub enum TyKind {
  Vec(P<Ty>),
  /// A path (`module::module::...::Type`), optionally
  /// "qualified", e.g. `<Vec<T> as SomeTrait>::SomeType`.
  ///
  /// Type parameters are stored in the Path itself
  Path(Path),
  Infer
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Unsafety {
  Unsafe,
  Normal,
}

impl fmt::Display for Unsafety {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    fmt::Display::fmt(match *self {
      Unsafety::Normal => "normal",
      Unsafety::Unsafe => "unsafe",
    }, f)
  }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Constness {
  Const,
  NotConst,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Expr {
  pub id: NodeId,
  pub node: ExprKind,
  pub span: Span,
  pub attrs: ThinAttributes
}

impl Expr {
  pub fn attrs(&self) -> &[Attribute] {
    match self.attrs {
      Some(ref b) => b,
      None => &[],
    }
  }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Field {
  pub ident: SpannedIdent,
  pub expr: P<Expr>,
  pub span: Span,
}

pub type SpannedIdent = Spanned<Ident>;

/// Limit types of a range (inclusive or exclusive)
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum RangeLimits {
  /// Inclusive at the beginning, exclusive at the end
  HalfOpen,
  /// Inclusive at the beginning and end
  Closed,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum ExprKind {
  /// A unary operation (For example: `!x`, `*x`)
  Unary(UnOp, P<Expr>),
  Binary(BinOp, P<Expr>, P<Expr>),
  Literal,
  /// A cast (`foo as f64`)
  Cast(P<Expr>, P<Ty>),
  Type(P<Expr>, P<Ty>),
  If(P<Expr>),
  /// An `if let` expression with an optional else block
  ///
  /// `if let pat = expr { block } else { expr }`
  ///
  /// This is desugared to a `match` expression.
  IfLet(P<Expr>),
  While,
  Loop,
  ForLoop,
  Match,
  Block,
  /// First expr is the place; second expr is the value.
  InPlace(P<Expr>, P<Expr>),
  /// An assignment (`a = foo()`)
  Assign(P<Expr>, P<Expr>),
  /// An assignment with an operator
  ///
  /// For example, `a += 1`.
  AssignOp(BinOp, P<Expr>, P<Expr>),

  Paren(P<Expr>),

  /// Variable reference, possibly containing `::` and/or type
  /// parameters, e.g. foo::bar::<baz>.
  ///
  /// Optionally "qualified",
  /// e.g. `<Vec<T> as SomeTrait>::SomeType`.
  Path(Option<QSelf>, Path),

  /// A struct literal expression.
  ///
  /// For example, `Foo {x: 1, y: 2}`, or
  /// `Foo {x: 1, .. base}`, where `base` is the `Option<Expr>`.
  Struct(Path, Vec<Field>, Option<P<Expr>>),

  /// A tuple (`(a, b, c ,d)`)
  Tup(Vec<P<Expr>>),

  /// Access of a named struct field (`obj.foo`)
  Field(P<Expr>, SpannedIdent),

  /// Access of an unnamed field of a struct or tuple-struct
  ///
  /// For example, `foo.0`.
  TupField(P<Expr>, Spanned<usize>),

  /// A range (`1..2`, `1..`, `..2`, `1...2`, `1...`, `...2`)
  Range(Option<P<Expr>>, Option<P<Expr>>, RangeLimits),

  /// An indexing operation (`foo[2]`)
  Index(P<Expr>, P<Expr>),

  /// A function call
  ///
  /// The first field resolves to the function itself,
  /// and the second field is the list of arguments
  Call(P<Expr>, Vec<P<Expr>>),

  /// A method call (`x.foo::<Bar, Baz>(a, b, c, d)`)
  ///
  /// The `SpannedIdent` is the identifier for the method name.
  /// The vector of `Ty`s are the ascripted type parameters for the method
  /// (within the angle brackets).
  ///
  /// The first element of the vector of `Expr`s is the expression that evaluates
  /// to the object on which the method is being called on (the receiver),
  /// and the remaining elements are the rest of the arguments.
  ///
  /// Thus, `x.foo::<Bar, Baz>(a, b, c, d)` is represented as
  /// `ExprKind::MethodCall(foo, [Bar, Baz], [x, a, b, c, d])`.
  MethodCall(SpannedIdent, Vec<P<Ty>>, Vec<P<Expr>>),

  /// A literal (For example: `1`, `"foo"`)
  Lit(P<Lit>),
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

pub type BinOp = Spanned<BinOpKind>;

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
  LShift,
  /// The `>>` operator (shift right)
  RShift,
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

impl BinOpKind {
  pub fn to_string(&self) -> &'static str {
    use self::BinOpKind::*;
    match *self {
      Add => "+",
      Sub => "-",
      Mul => "*",
      Div => "/",
      Rem => "%",
      And => "&&",
      Or => "||",
      BitXor => "^",
      BitAnd => "&",
      BitOr => "|",
      LShift => "<<",
      RShift => ">>",
      Eq => "==",
      Lt => "<",
      Le => "<=",
      Ne => "!=",
      Ge => ">=",
      Gt => ">",
    }
  }
  pub fn lazy(&self) -> bool {
    match *self {
      BinOpKind::And | BinOpKind::Or => true,
      _ => false
    }
  }

  pub fn is_shift(&self) -> bool {
    match *self {
      BinOpKind::LShift | BinOpKind::RShift => true,
      _ => false
    }
  }
  pub fn is_comparison(&self) -> bool {
    use self::BinOpKind::*;
    match *self {
      Eq | Lt | Le | Ne | Gt | Ge =>
        true,
      And | Or | Add | Sub | Mul | Div | Rem |
      BitXor | BitAnd | BitOr | LShift | RShift =>
        false,
    }
  }
  /// Returns `true` if the binary operator takes its arguments by value
  pub fn is_by_value(&self) -> bool {
    !self.is_comparison()
  }
}

#[derive(Clone, PartialEq, Eq, Hash, Copy)]
pub enum IntTy {
  Is,
  I8,
  I16,
  I32,
  I64,
}

impl fmt::Debug for IntTy {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    fmt::Display::fmt(self, f)
  }
}

impl fmt::Display for IntTy {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.ty_to_string())
  }
}

impl IntTy {
  pub fn ty_to_string(&self) -> &'static str {
    match *self {
      IntTy::Is => "isize",
      IntTy::I8 => "i8",
      IntTy::I16 => "i16",
      IntTy::I32 => "i32",
      IntTy::I64 => "i64"
    }
  }

  pub fn val_to_string(&self, val: i64) -> String {
    // cast to a u64 so we can correctly print INT64_MIN. All integral types
    // are parsed as u64, so we wouldn't want to print an extra negative
    // sign.
    format!("{}{}", val as u64, self.ty_to_string())
  }

  pub fn ty_max(&self) -> u64 {
    match *self {
      IntTy::I8 => 0x80,
      IntTy::I16 => 0x8000,
      IntTy::Is | IntTy::I32 => 0x80000000, // FIXME: actually ni about Is
      IntTy::I64 => 0x8000000000000000
    }
  }

  pub fn bit_width(&self) -> Option<usize> {
    Some(match *self {
      IntTy::Is => return None,
      IntTy::I8 => 8,
      IntTy::I16 => 16,
      IntTy::I32 => 32,
      IntTy::I64 => 64,
    })
  }
}

#[derive(Clone, PartialEq, Eq, Hash, Copy)]
pub enum UintTy {
  Us,
  U8,
  U16,
  U32,
  U64,
}

impl UintTy {
  pub fn ty_to_string(&self) -> &'static str {
    match *self {
      UintTy::Us => "usize",
      UintTy::U8 => "u8",
      UintTy::U16 => "u16",
      UintTy::U32 => "u32",
      UintTy::U64 => "u64"
    }
  }

  pub fn val_to_string(&self, val: u64) -> String {
    format!("{}{}", val, self.ty_to_string())
  }

  pub fn ty_max(&self) -> u64 {
    match *self {
      UintTy::U8 => 0xff,
      UintTy::U16 => 0xffff,
      UintTy::Us | UintTy::U32 => 0xffffffff, // FIXME: actually ni about Us
      UintTy::U64 => 0xffffffffffffffff
    }
  }

  pub fn bit_width(&self) -> Option<usize> {
    Some(match *self {
      UintTy::Us => return None,
      UintTy::U8 => 8,
      UintTy::U16 => 16,
      UintTy::U32 => 32,
      UintTy::U64 => 64,
    })
  }
}

impl fmt::Debug for UintTy {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    fmt::Display::fmt(self, f)
  }
}

impl fmt::Display for UintTy {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.ty_to_string())
  }
}

#[derive(Clone, PartialEq, Eq, Hash, Copy)]
pub enum FloatTy {
  F32,
  F64,
}

impl fmt::Debug for FloatTy {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    fmt::Display::fmt(self, f)
  }
}

impl fmt::Display for FloatTy {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.ty_to_string())
  }
}

impl FloatTy {
  pub fn ty_to_string(&self) -> &'static str {
    match *self {
      FloatTy::F32 => "f32",
      FloatTy::F64 => "f64",
    }
  }

  pub fn bit_width(&self) -> usize {
    match *self {
      FloatTy::F32 => 32,
      FloatTy::F64 => 64,
    }
  }
}

/// The explicit Self type in a "qualified path". The actual
/// path, including the trait and the associated item, is stored
/// separately. `position` represents the index of the associated
/// item qualified with this Self type.
///
/// ```ignore
/// <Vec<T> as a::b::Trait>::AssociatedItem
///  ^~~~~     ~~~~~~~~~~~~~~^
///  ty        position = 3
///
/// <Vec<T>>::AssociatedItem
///  ^~~~~    ^
///  ty       position = 0
/// ```
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct QSelf {
  pub ty: P<Ty>,
  pub position: usize
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub enum StrStyle {
  /// A regular string, like `"foo"`
  Cooked,
  /// A raw string, like `r##"foo"##`
  ///
  /// The uint is the number of `#` symbols used
  Raw(usize)
}

/// A literal
pub type Lit = Spanned<LitKind>;

#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub enum LitIntType {
  Signed(IntTy),
  Unsigned(UintTy),
  Unsuffixed,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum LitKind {
  /// A string literal (`"foo"`)
  Str(InternedString, StrStyle),
  /// A byte string (`b"foo"`)
  ByteStr(Rc<Vec<u8>>),
  /// A byte char (`b'f'`)
  Byte(u8),
  /// A character literal (`'a'`)
  Char(char),
  /// An integer literal (`1`)
  Int(u64, LitIntType),
  /// A float literal (`1f64` or `1E10f64`)
  Float(InternedString, FloatTy),
  /// A float literal without a suffix (`1.0 or 1.0E10`)
  FloatUnsuffixed(InternedString),
  /// A boolean literal
  Bool(bool),
}

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