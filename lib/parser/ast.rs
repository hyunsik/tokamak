use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use std::u32;

use rustc_serialize::{Decodable, Decoder, Encodable, Encoder};

pub use self::ViewPath_::*;
pub use self::Mutability::*;
pub use symbol::Symbol as Name;

use abi::Abi;
use ast_printer as printer;
use codemap::Spanned;
use common::codespan::{DUMMY_SPAN, Span};
use comments::{doc_comment_style, strip_doc_comment_decoration};
use hygiene::SyntaxContext;
use ptr::P;
use symbol::{Symbol, keywords};
use util::ThinVec;

/// An identifier contains a Name (index into the interner
/// table) and a SyntaxContext to track renaming and
/// macro expansion per Flatt et al., "Macros That Work Together"
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ident {
  pub name: Name,
  pub ctxt: SyntaxContext
}

impl Ident {
    pub const fn with_empty_ctxt(name: Name) -> Ident {
        Ident { name: name, ctxt: SyntaxContext::empty() }
    }

    /// Maps a string to an identifier with an empty syntax context.
    pub fn from_str(s: &str) -> Ident {
        Ident::with_empty_ctxt(Symbol::intern(s))
    }

    pub fn unhygienize(&self) -> Ident {
        Ident { name: self.name, ctxt: SyntaxContext::empty() }
    }
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{:?}", self.name, self.ctxt)
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.name, f)
    }
}

impl Encodable for Ident {
    fn encode<S: Encoder>(&self, s: &mut S) -> Result<(), S::Error> {
        self.name.encode(s)
    }
}

impl Decodable for Ident {
    fn decode<D: Decoder>(d: &mut D) -> Result<Ident, D::Error> {
        Ok(Ident::with_empty_ctxt(Name::decode(d)?))
    }
}

pub type ViewPath = Spanned<ViewPath_>;

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub enum ViewPath_ {

  /// `foo::bar::baz as quux`
  ///
  /// or just
  ///
  /// `foo::bar::baz` (with `as baz` implicitly on the right)
  ViewPathSimple(Ident, Path),

  /// `foo::bar::*`
  ViewPathGlob(Path),

  /// `foo::bar::{a,b,c}`
  ViewPathList(Path, Vec<PathListItem>)
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug, Copy)]
pub enum PathListItemKind {
  Ident {
    name: Ident,
    /// renamed in list, eg `use foo::{bar as baz};`
    rename: Option<Ident>,
    id: NodeId
  },
  Mod {
    /// renamed in list, eg `use foo::{self as baz};`
    rename: Option<Ident>,
    id: NodeId
  }
}

impl PathListItemKind {
  pub fn id(&self) -> NodeId {
    match *self {
      PathListItemKind::Ident { id, .. } | PathListItemKind::Mod { id, .. } => id
    }
  }

  pub fn name(&self) -> Option<Ident> {
    match *self {
      PathListItemKind::Ident { name, .. } => Some(name),
      PathListItemKind::Mod { .. } => None,
    }
  }

  pub fn rename(&self) -> Option<Ident> {
    match *self {
      PathListItemKind::Ident { rename, .. } | PathListItemKind::Mod { rename, .. } => rename
    }
  }
}

pub type PathListItem = Spanned<PathListItemKind>;

/// A "Path" is essentially Rust's notion of a name.
///
/// It's represented as a sequence of identifiers,
/// along with a bunch of supporting information.
///
/// E.g. `std::cmp::PartialEq`
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash)]
pub struct Path {
  pub span: Span,
  /// The segments in the path: the things separated by `::`.
  pub segments: Vec<PathSegment>,
}

impl fmt::Debug for Path {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "path({})", printer::path_to_string(self))
  }
}

impl fmt::Display for Path {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", printer::path_to_string(self))
  }
}

impl Path {
    // convert a span and an identifier to the corresponding
    // 1-segment path
    pub fn from_ident(s: Span, identifier: Ident) -> Path {
        Path {
            span: s,
            segments: vec![identifier.into()],
        }
    }

    pub fn default_to_global(mut self) -> Path {
        let name = self.segments[0].identifier.name;
        if !self.is_global() && name != "$package" &&
           name != keywords::SelfValue.name() && name != keywords::Super.name() {
            self.segments.insert(0, PathSegment::package_root());
        }
        self
    }

    pub fn is_global(&self) -> bool {
        !self.segments.is_empty() && self.segments[0].identifier.name == keywords::PackageRoot.name()
    }
}

/// A segment of a path: an identifier, an optional lifetime, and a set of types.
///
/// E.g. `std`, `String` or `Box<T>`
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct PathSegment {
  /// The identifier portion of this path segment.
  pub identifier: Ident
}

impl From<Ident> for PathSegment {
    fn from(id: Ident) -> Self {
        PathSegment { identifier: id }
    }
}

impl PathSegment {
    pub fn package_root() -> Self {
        PathSegment {
            identifier: keywords::PackageRoot.ident(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash, Debug,
         RustcEncodable, RustcDecodable)]
pub struct NodeId(u32);

impl NodeId {
    pub fn new(x: usize) -> NodeId {
        assert!(x < (u32::MAX as usize));
        NodeId(x as u32)
    }

    pub fn from_u32(x: u32) -> NodeId {
        NodeId(x)
    }

    pub fn as_usize(&self) -> usize {
        self.0 as usize
    }

    pub fn as_u32(&self) -> u32 {
        self.0
    }
}

impl fmt::Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

/// Node id used to represent the root of the crate.
pub const PACKAGE_NODE_ID: NodeId = NodeId(0);

/// When parsing and doing expansions, we initially give all AST nodes this AST
/// node value. Then later, in the renumber pass, we renumber them to have
/// small, positive ids.
pub const DUMMY_NODE_ID: NodeId = NodeId(!0);

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct TyParam {
    pub attrs: ThinVec<Attribute>,
    pub ident: Ident,
    pub id: NodeId,
    pub default: Option<P<Ty>>,
    pub span: Span,
}

/// Represents lifetimes and type parameters attached to a declaration
/// of a function, enum, trait, etc.
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct Generics {
    pub ty_params: P<[TyParam]>,
    pub span: Span,
}

impl Generics {
    pub fn span_for_name(&self, name: &str) -> Option<Span> {
        for t in &self.ty_params {
            if t.ident.name == name {
                return Some(t.span);
            }
        }
        None
    }
}

impl Default for Generics {
    /// Creates an instance of `Generics`.
    fn default() ->  Generics {
        Generics {
            ty_params: P::new(),
            span: DUMMY_SPAN,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Package {
  pub module: Module,
  pub span: Span,
  pub attrs: Vec<Attribute>,
}

/// Module declaration.
///
/// E.g. `mod foo;` or `mod foo { .. }`
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct Module {
  /// A span from the first token past `{` to the last token until `}`.
  /// For `mod foo;`, the inner span ranges from the first token
  /// to the last token in the external file.
  pub inner: Span,
  pub items: Vec<P<Item>>
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub enum Visibility {
    Public,
    Crate(Span),
    Restricted { path: P<Path>, id: NodeId },
    Inherited,
}

/// An item
///
/// The name might be a dummy name in case of anonymous items
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct Item {
  pub ident: Ident,
  pub attrs: Vec<Attribute>,
  pub id: NodeId,
  pub node: ItemKind,
  pub vis: Visibility,
  pub span: Span,
}

impl Item {
  pub fn attrs(&self) -> &[Attribute] {
    &self.attrs
  }
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub enum ItemKind {
  /// A `use` or `pub use` item
  Import(P<ViewPath>),
  /// An external module
  ForeignMod(ForeignMod),
  /// A `static` item
  Static(P<Ty>, Mutability, P<Expr>),
  /// A `const` item
  Const(P<Ty>, P<Expr>),
  /// A function declaration
  Fn(P<FnDecl>, Unsafety, Spanned<Constness>, Abi, Generics, P<Block>),
  /// A type alias (`type` or `pub type`).
  ///
  /// E.g. `type Foo = Bar<u8>;`
  Ty(P<Ty>, Generics),
  Enum,
  Struct,
}

/// Foreign module declaration.
///
/// E.g. `extern { .. }` or `extern C { .. }`
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct ForeignMod {
  pub abi: Abi,
  pub items: Vec<ForeignItem>,
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct ForeignItem {
  pub ident: Ident,
  pub attrs: Vec<Attribute>,
  pub node: ForeignItemKind,
  pub id: NodeId,
  pub span: Span,
  pub vis: Visibility,
}

/// An item within an `extern` block
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub enum ForeignItemKind {
  /// A foreign function
  Fn(P<FnDecl>, Generics),
  /// A foreign static item (`static ext: u8`), with optional mutability
  /// (the boolean is true when mutable)
  Static(P<Ty>, bool),
}

impl ForeignItemKind {
    pub fn descriptive_variant(&self) -> &str {
        match *self {
            ForeignItemKind::Fn(..) => "foreign function",
            ForeignItemKind::Static(..) => "foreign static item"
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
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

#[derive(Copy, Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub enum Constness {
  Const,
  NotConst,
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash)]
pub struct Ty {
  pub id: NodeId,
  pub node: TyKind,
  pub span: Span,
}

impl fmt::Debug for Ty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "type({})", printer::ty_to_string(self))
    }
}

/// The different kinds of types recognized by the compiler
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub enum TyKind {
  /// A variable-length slice (`[T]`)
  Slice(P<Ty>),
  /// A fixed length array (`[T; n]`)
  Array(P<Ty>, P<Expr>),
  /// A tuple (`(A, B, C, D,...)`)
  Tup(Vec<P<Ty>> ),
  /// A path (`module::module::...::Type`), optionally
  /// "qualified", e.g. `<Vec<T> as SomeTrait>::SomeType`.
  ///
  /// Type parameters are stored in the Path itself
  Path(Path),
  /// No-op; kept solely so that we can pretty-print faithfully
  Paren(P<Ty>),
  /// TyKind::Infer means the type should be inferred instead of it having been
  /// specified. This can appear anywhere in a type.
  Infer
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug, Copy)]
pub enum BlockCheckMode {
  Default,
  Unsafe(UnsafeSource),
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug, Copy)]
pub enum UnsafeSource {
  CompilerGenerated,
  UserProvided,
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct Block {
  /// Statements in a block
  pub stmts: Vec<Stmt>,
  pub id: NodeId,
  /// Distinguishes between `unsafe { ... }` and `{ ... }`
  pub rules: BlockCheckMode,
  pub span: Span,
}

/// A statement
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash)]
pub struct Stmt {
  pub id: NodeId,
  pub node: StmtKind,
  pub span: Span,
}

impl Stmt {
  pub fn add_trailing_semicolon(mut self) -> Self {
    self.node = match self.node {
      StmtKind::Expr(expr) => StmtKind::Semi(expr),
      node @ _ => node,
    };
    self
  }
}

impl fmt::Debug for Stmt {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "stmt({}: {})", self.id.to_string(), printer::stmt_to_string(self))
  }
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash)]
pub enum StmtKind {
  /// A local (let) binding.
  Local(P<Local>),

  /// An item definition.
  Item(P<Item>),

  /// Expr without trailing semi-colon.
  Expr(P<Expr>),

  Semi(P<Expr>),
}

// FIXME (pending discussion of #1697, #2178...): local should really be
// a refinement on pat.
/// Local represents a `let` statement, e.g., `let <pat>:<ty> = <expr>;`
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct Local {
  pub pat: P<Pat>,
  pub ty: Option<P<Ty>>,
  pub mutbl: Mutability,
  /// Initializer expression to set the value, if any
  pub init: Option<P<Expr>>,
  pub id: NodeId,
  pub span: Span,
  pub attrs: ThinVec<Attribute>,
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash)]
pub struct Pat {
  pub id: NodeId,
  pub node: PatKind,
  pub span: Span,
}

impl fmt::Debug for Pat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "pat({}: {})", self.id, printer::pat_to_string(self))
    }
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub enum PatKind {
  /// Represents a wildcard pattern (`_`)
  Wild,

  /// A `PatKind::Ident` may either be a new bound variable,
  /// or a unit struct/variant pattern, or a const pattern (in the last two cases
  /// the third field must be `None`).
  ///
  /// In the unit or const pattern case, the parser can't determine
  /// which it is. The resolver determines this, and
  /// records this pattern's `NodeId` in an auxiliary
  /// set (of "PatIdents that refer to unit patterns or constants").
  Ident(BindingMode, SpannedIdent, Option<P<Pat>>),

  /// A path pattern.
  /// Such pattern can be resolved to a unit struct/variant or a constant.
  Path(Path),

  /// An associated const named using the qualified path `<T>::CONST` or
  /// `<T as Trait>::CONST`. Associated consts from inherent impls can be
  /// referred to as simply `T::CONST`, in which case they will end up as
  /// PatKind::Path, and the resolver will have to sort that out.
  QPath(QSelf, Path),

  /// A struct or struct variant pattern, e.g. `Variant {x, y, ..}`.
  /// The `bool` is `true` in the presence of a `..`.
  Struct(Path, Vec<Spanned<FieldPat>>, bool),

  /// A tuple pattern `(a, b)`.
  /// If the `..` pattern fragment is present, then `Option<usize>` denotes its position.
  /// 0 <= position <= subpats.len()
  Tuple(Vec<P<Pat>>, Option<usize>),

  /// A tuple struct/variant pattern `Variant(x, y, .., z)`.
  /// If the `..` pattern fragment is present, then `Option<usize>` denotes its position.
  /// 0 <= position <= subpats.len()
  TupleStruct(Path, Vec<P<Pat>>, Option<usize>),

  /// `[a, b, ..i, y, z]` is represented as:
  ///     `PatKind::Vec(box [a, b], Some(i), box [y, z])`
  Vec(Vec<P<Pat>>, Option<P<Pat>>, Vec<P<Pat>>),

  /// A range pattern, e.g. `1...2`
  Range(P<Expr>, P<Expr>),

  /// A literal
  Lit(P<Expr>),
}

/// A single field in a struct pattern
///
/// Patterns like the fields of Foo `{ x, ref y, ref mut z }`
/// are treated the same as` x: x, y: ref y, z: ref mut z`,
/// except is_shorthand is true
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct FieldPat {
  /// The identifier for the field
  pub ident: Ident,
  /// The pattern the field is destructured to
  pub pat: P<Pat>,
  pub is_shorthand: bool,
}

/// An arm of a 'match'.
///
/// E.g. `0...10 => { println!("match!") }` as in
///
/// ```rust,ignore
/// match n {
///     0...10 => { println!("match!") },
///     // ..
/// }
/// ```
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct Arm {
  pub attrs: Vec<Attribute>,
  pub pats: Vec<P<Pat>>,
  pub guard: Option<P<Expr>>,
  pub body: P<Expr>,
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct Field {
  pub ident: SpannedIdent,
  pub expr: P<Expr>,
  pub span: Span,
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug, Copy)]
pub enum Mutability {
  Mutable,
  Immutable,
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug, Copy)]
pub enum BindingMode {
  ByRef,
  ByValue,
}

/// An expression
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash,)]
pub struct Expr {
  pub id: NodeId,
  pub node: ExprKind,
  pub span: Span,
  pub attrs: ThinVec<Attribute>
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "expr({}: {})", self.id, printer::expr_to_string(self))
    }
}

pub type SpannedIdent = Spanned<Ident>;

/// Limit types of a range (inclusive or exclusive)
#[derive(Copy, Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub enum RangeLimits {
  /// Inclusive at the beginning, exclusive at the end
  HalfOpen,
  /// Inclusive at the beginning and end
  Closed,
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub enum ExprKind {
  /// A unary operation (For example: `!x`, `*x`)
  Unary(UnOp, P<Expr>),

  Binary(BinOp, P<Expr>, P<Expr>),

  /// A cast (`foo as f64`)
  Cast(P<Expr>, P<Ty>),

  Type(P<Expr>, P<Ty>),

  /// A closure (for example, `move |a, b, c| {a + b + c}`)
  ///
  /// The final span is the span of the argument block `|...|`
  Closure(CaptureBy, P<FnDecl>, P<Expr>, Span),

  /// A block (`{ ... }`)
  Block(P<Block>),

  /// First expr is the place; second expr is the value.
  InPlace(P<Expr>, P<Expr>),

  /// An assignment (`a = foo()`)
  Assign(P<Expr>, P<Expr>),

  /// An assignment with an operator
  ///
  /// For example, `a += 1`.
  AssignOp(BinOp, P<Expr>, P<Expr>),

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

  /// An indexing operation (`foo[2]`)
  Index(P<Expr>, P<Expr>),

  /// A range (`1..2`, `1..`, `..2`, `1...2`, `1...`, `...2`)
  Range(Option<P<Expr>>, Option<P<Expr>>, RangeLimits),

  /// An array (`[a, b, c, d]`)
  Vec(Vec<P<Expr>>),

  /// An array literal constructed from one repeated element.
  ///
  /// For example, `[1; 5]`. The first expression is the element
  /// to be repeated; the second is the number of times to repeat it.
  Repeat(P<Expr>, P<Expr>),

  /// A literal (For example: `1`, `"foo"`)
  Lit(P<Lit>),

  /// No-op: used solely so we can pretty-print faithfully
  Paren(P<Expr>),

  //--------------------------------------------------------------------------
  // Function Call
  //--------------------------------------------------------------------------

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

  //--------------------------------------------------------------------------
  // Expression for Control Flows
  //--------------------------------------------------------------------------

  /// A `match` block.
  Match(P<Expr>, Vec<Arm>),

  /// An `if` block, with an optional else block
  ///
  /// `if expr { block } else { expr }`
  If(P<Expr>, P<Block>, Option<P<Expr>>),

  /// An `if let` expression with an optional else block
  ///
  /// `if let pat = expr { block } else { expr }`
  ///
  /// This is desugared to a `match` expression.
  IfLet(P<Pat>, P<Expr>, P<Block>, Option<P<Expr>>),

  /// A while loop, with an optional label
  ///
  /// `'label: while expr { block }`
  While(P<Expr>, P<Block>, Option<SpannedIdent>),

  /// A while-let loop, with an optional label
  ///
  /// `'label: while let pat = expr { block }`
  ///
  /// This is desugared to a combination of `loop` and `match` expressions.
  WhileLet(P<Pat>, P<Expr>, P<Block>, Option<SpannedIdent>),

  /// A for loop, with an optional label
  ///
  /// `'label: for pat in expr { block }`
  ///
  /// This is desugared to a combination of `loop` and `match` expressions.
  ForLoop(P<Pat>, P<Expr>, P<Block>, Option<SpannedIdent>),

  /// Conditionless loop (can be exited with break, continue, or return)
  ///
  /// `'label: loop { block }`
  Loop(P<Block>, Option<SpannedIdent>),

  /// A `break`, with an optional label to break
  Break(Option<SpannedIdent>),

  /// A `continue`, with an optional label
  Continue(Option<SpannedIdent>),

  /// A `return`, with an optional value to be returned
  Ret(Option<P<Expr>>),
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug, Copy)]
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

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug, Copy)]
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

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Copy)]
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

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Copy)]
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

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Copy)]
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
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct QSelf {
  pub ty: P<Ty>,
  pub position: usize
}

/// A capture clause
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug, Copy)]
pub enum CaptureBy {
  Value,
  Ref,
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug, Copy)]
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

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug, Copy)]
pub enum LitIntType {
  Signed(IntTy),
  Unsigned(UintTy),
  Unsuffixed,
}

/// Literal kind.
///
/// E.g. `"foo"`, `42`, `12.34` or `bool`
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub enum LitKind {
  /// A string literal (`"foo"`)
  Str(Symbol, StrStyle),
  /// A byte string (`b"foo"`)
  ByteStr(Rc<Vec<u8>>),
  /// A byte char (`b'f'`)
  Byte(u8),
  /// A character literal (`'a'`)
  Char(char),
  /// An integer literal (`1`)
  Int(u64, LitIntType),
  /// A float literal (`1f64` or `1E10f64`)
  Float(Symbol, FloatTy),
  /// A float literal without a suffix (`1.0 or 1.0E10`)
  FloatUnsuffixed(Symbol),
  /// A boolean literal
  Bool(bool),
}

/// Distinguishes between Attributes that decorate items and Attributes that
/// are contained as statements within items. These two cases need to be
/// distinguished for pretty-printing.
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug, Copy)]
pub enum AttrStyle {
  Outer,
  Inner,
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug, Copy)]
pub struct AttrId(pub usize);

/// Meta-data associated with an item
/// Doc-comments are promoted to attributes that have is_sugared_doc = true
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct Attribute {
    pub id: AttrId,
    pub style: AttrStyle,
    pub value: MetaItem,
    pub is_sugared_doc: bool,
    pub span: Span,
}

/// A spanned compile-time attribute list item.
pub type NestedMetaItem = Spanned<NestedMetaItemKind>;

/// Possible values inside of compile-time attribute lists.
///
/// E.g. the '..' in `#[name(..)]`.
#[derive(Clone, Eq, RustcEncodable, RustcDecodable, Hash, Debug, PartialEq)]
pub enum NestedMetaItemKind {
    /// A full MetaItem, for recursive meta items.
    MetaItem(MetaItem),
    /// A literal.
    ///
    /// E.g. "foo", 64, true
    Literal(Lit),
}

/// A spanned compile-time attribute item.
///
/// E.g. `#[test]`, `#[derive(..)]` or `#[feature = "foo"]`
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct MetaItem {
    pub name: Name,
    pub node: MetaItemKind,
    pub span: Span,
}

/// A compile-time attribute item.
///
/// E.g. `#[test]`, `#[derive(..)]` or `#[feature = "foo"]`
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub enum MetaItemKind {
    /// Word meta item.
    ///
    /// E.g. `test` as in `#[test]`
    Word,
    /// List meta item.
    ///
    /// E.g. `derive(..)` as in `#[derive(..)]`
    List(Vec<NestedMetaItem>),
    /// Name value meta item.
    ///
    /// E.g. `feature = "foo"` as in `#[feature = "foo"]`
    NameValue(Lit)
}

/// Header (not the body) of a function declaration.
///
/// E.g. `fn foo(bar: baz)`
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct FnDecl {
  pub inputs: Vec<Arg>,
  pub output: FunctionRetTy,
  pub variadic: bool
}

/// An argument in a function header.
///
/// E.g. `bar: usize` as in `fn foo(bar: usize)`
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct Arg {
  pub ty: P<Ty>,
  pub pat: P<Pat>,
  pub id: NodeId,
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub enum FunctionRetTy {
  /// Functions with return type `!`that always
  /// raise an error or exit (i.e. never return to the caller)
  None(Span),
  /// Return type is not specified.
  ///
  /// Functions default to `()` and
  /// closures default to inference. Span points to where return
  /// type would be inserted.
  Default(Span),
  /// Everything else
  Ty(P<Ty>),
}

impl FunctionRetTy {
  pub fn span(&self) -> Span {
    match *self {
      FunctionRetTy::None(span) => span,
      FunctionRetTy::Default(span) => span,
      FunctionRetTy::Ty(ref ty) => ty.span,
    }
  }
}