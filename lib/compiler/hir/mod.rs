mod def;
mod def_id;
mod print;

use std::fmt;

use parser::abi::Abi;
use common::codespan::Span;
use parser::ast::{Name, NodeId};
use parser::ast::{Attribute, Lit, StrStyle, FloatTy, IntTy, UintTy, MetaItem};
use parser::codemap::{self, DUMMY_SPAN, Spanned};
use parser::ptr::P;
use parser::token::keywords;
use parser::util::ThinVec;

use self::def::Def;
pub use self::Decl_::*;
pub use self::BinOp_::*;
pub use self::UnOp::*;
pub use self::Stmt_::*;
pub use self::FunctionRetTy::*;
pub use self::ForeignItem_::*;

/// HIR doesn't commit to a concrete storage type and have its own alias for a vector.
/// It can be `Vec`, `P<[T]>` or potentially `Box<[T]>`, or some other container with similar
/// behavior. Unlike AST, HIR is mostly a static structure, so we can use an owned slice instead
/// of `Vec` to avoid keeping extra capacity.
pub type HirVec<T> = P<[T]>;

macro_rules! hir_vec {
    ($elem:expr; $n:expr) => (
        $crate::hir::HirVec::from(vec![$elem; $n])
    );
    ($($x:expr),*) => (
        $crate::hir::HirVec::from(vec![$($x),*])
    );
    ($($x:expr,)*) => (hir_vec![$($x),*])
}

/// A "Path" is an identifier for all things available in the program;
/// for instance: std::cmp::PartialEq  .  It's represented as a sequence of
/// identifiers, along with a bunch of supporting information.
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash)]
pub struct Path {
    pub span: Span,
    /// The definition that the path resolved to.
    pub def: Def,
    /// The segments in the path: the things separated by `::`.
    pub segments: HirVec<PathSegment>,
}

impl Path {
    pub fn is_global(&self) -> bool {
        !self.segments.is_empty() && self.segments[0].name == keywords::PackageRoot.name()
    }
}

impl fmt::Debug for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "path({})",
               print::to_string(print::NO_ANN, |s| s.print_path(self, false)))
    }
}

/// A segment of a path: an identifier, an optional lifetime, and a set of
/// types.
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct PathSegment {
    /// The identifier portion of this path segment.
    pub name: Name,
}

impl PathSegment {
    /// Convert an identifier to the corresponding segment.
    pub fn from_name(name: Name) -> PathSegment {
        PathSegment {
            name: name,
        }
    }
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct TyParam {
    pub name: Name,
    pub id: NodeId,
    pub default: Option<P<Ty>>,
    pub span: Span,
    pub pure_wrt_drop: bool,
}

/// Represents lifetimes and type parameters attached to a declaration
/// of a function, enum, trait, etc.
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct Generics {
    pub ty_params: HirVec<TyParam>,
    pub span: Span,
}

impl Generics {
    pub fn empty() -> Generics {
        Generics {
            ty_params: HirVec::new(),
            span: DUMMY_SPAN,
        }
    }

    pub fn is_type_parameterized(&self) -> bool {
        !self.ty_params.is_empty()
    }

    pub fn is_parameterized(&self) -> bool {
        self.is_type_parameterized()
    }
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct Block {
    /// Statements in a block
    pub stmts: HirVec<Stmt>,
    /// An expression at the end of the block
    /// without a semicolon, if any
    pub expr: Option<P<Expr>>,
    pub id: NodeId,
    /// Distinguishes between `unsafe { ... }` and `{ ... }`
    pub rules: BlockCheckMode,
    pub span: Span,
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash)]
pub struct Pat {
    pub id: NodeId,
    pub node: PatKind,
    pub span: Span,
}

impl fmt::Debug for Pat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "pat({}: {})", self.id,
               print::to_string(print::NO_ANN, |s| s.print_pat(self)))
    }
}

/// A single field in a struct pattern
///
/// Patterns like the fields of Foo `{ x, ref y, ref mut z }`
/// are treated the same as` x: x, y: ref y, z: ref mut z`,
/// except is_shorthand is true
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct FieldPat {
    /// The identifier for the field
    pub name: Name,
    /// The pattern the field is destructured to
    pub pat: P<Pat>,
    pub is_shorthand: bool,
}


#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub enum PatKind {
    /// Represents a wildcard pattern (`_`)
    Wild,

    /// A struct or struct variant pattern, e.g. `Variant {x, y, ..}`.
    /// The `bool` is `true` in the presence of a `..`.
    Struct(QPath, HirVec<Spanned<FieldPat>>, bool),

    /// A tuple struct/variant pattern `Variant(x, y, .., z)`.
    /// If the `..` pattern fragment is present, then `Option<usize>` denotes its position.
    /// 0 <= position <= subpats.len()
    TupleStruct(QPath, HirVec<P<Pat>>, Option<usize>),

    /// A path pattern for an unit struct/variant or a (maybe-associated) constant.
    Path(QPath),

    /// A tuple pattern `(a, b)`.
    /// If the `..` pattern fragment is present, then `Option<usize>` denotes its position.
    /// 0 <= position <= subpats.len()
    Tuple(HirVec<P<Pat>>, Option<usize>),
    /// A `box` pattern
    Box(P<Pat>),
    /// A reference pattern, e.g. `&mut (a, b)`
    Ref(P<Pat>, Mutability),
    /// A literal
    Lit(P<Expr>),
    /// A range pattern, e.g. `1...2`
    Range(P<Expr>, P<Expr>),
    /// `[a, b, ..i, y, z]` is represented as:
    ///     `PatKind::Slice(box [a, b], Some(i), box [y, z])`
    Slice(HirVec<P<Pat>>, Option<P<Pat>>, HirVec<P<Pat>>),
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug, Copy)]
pub enum Mutability {
    MutMutable,
    MutImmutable,
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug, Copy)]
pub enum BinOp_ {
    /// The `+` operator (addition)
    BiAdd,
    /// The `-` operator (subtraction)
    BiSub,
    /// The `*` operator (multiplication)
    BiMul,
    /// The `/` operator (division)
    BiDiv,
    /// The `%` operator (modulus)
    BiRem,
    /// The `&&` operator (logical and)
    BiAnd,
    /// The `||` operator (logical or)
    BiOr,
    /// The `^` operator (bitwise xor)
    BiBitXor,
    /// The `&` operator (bitwise and)
    BiBitAnd,
    /// The `|` operator (bitwise or)
    BiBitOr,
    /// The `<<` operator (shift left)
    BiShl,
    /// The `>>` operator (shift right)
    BiShr,
    /// The `==` operator (equality)
    BiEq,
    /// The `<` operator (less than)
    BiLt,
    /// The `<=` operator (less than or equal to)
    BiLe,
    /// The `!=` operator (not equal to)
    BiNe,
    /// The `>=` operator (greater than or equal to)
    BiGe,
    /// The `>` operator (greater than)
    BiGt,
}

impl BinOp_ {
    pub fn as_str(self) -> &'static str {
        match self {
            BiAdd => "+",
            BiSub => "-",
            BiMul => "*",
            BiDiv => "/",
            BiRem => "%",
            BiAnd => "&&",
            BiOr => "||",
            BiBitXor => "^",
            BiBitAnd => "&",
            BiBitOr => "|",
            BiShl => "<<",
            BiShr => ">>",
            BiEq => "==",
            BiLt => "<",
            BiLe => "<=",
            BiNe => "!=",
            BiGe => ">=",
            BiGt => ">",
        }
    }

    pub fn is_lazy(self) -> bool {
        match self {
            BiAnd | BiOr => true,
            _ => false,
        }
    }

    pub fn is_shift(self) -> bool {
        match self {
            BiShl | BiShr => true,
            _ => false,
        }
    }

    pub fn is_comparison(self) -> bool {
        match self {
            BiEq | BiLt | BiLe | BiNe | BiGt | BiGe => true,
            BiAnd |
            BiOr |
            BiAdd |
            BiSub |
            BiMul |
            BiDiv |
            BiRem |
            BiBitXor |
            BiBitAnd |
            BiBitOr |
            BiShl |
            BiShr => false,
        }
    }

    /// Returns `true` if the binary operator takes its arguments by value
    pub fn is_by_value(self) -> bool {
        !self.is_comparison()
    }
}

pub type BinOp = Spanned<BinOp_>;

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug, Copy)]
pub enum UnOp {
    /// The `*` operator for dereferencing
    UnDeref,
    /// The `!` operator for logical inversion
    UnNot,
    /// The `-` operator for negation
    UnNeg,
}

impl UnOp {
    pub fn as_str(self) -> &'static str {
        match self {
            UnDeref => "*",
            UnNot => "!",
            UnNeg => "-",
        }
    }

    /// Returns `true` if the unary operator takes its argument by value
    pub fn is_by_value(self) -> bool {
        match self {
            UnNeg | UnNot => true,
            _ => false,
        }
    }
}

/// A statement
pub type Stmt = Spanned<Stmt_>;

impl fmt::Debug for Stmt_ {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Sadness.
        let spanned = codemap::dummy_spanned(self.clone());
        write!(f,
               "stmt({}: {})",
               spanned.node.id(),
               print::to_string(print::NO_ANN, |s| s.print_stmt(&spanned)))
    }
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash)]
pub enum Stmt_ {
    /// Could be an item or a local (let) binding:
    StmtDecl(P<Decl>, NodeId),

    /// Expr without trailing semi-colon (must have unit type):
    StmtExpr(P<Expr>, NodeId),

    /// Expr with trailing semi-colon (may have any type):
    StmtSemi(P<Expr>, NodeId),
}

impl Stmt_ {
    pub fn attrs(&self) -> &[Attribute] {
        match *self {
            StmtDecl(ref d, _) => d.node.attrs(),
            StmtExpr(ref e, _) |
            StmtSemi(ref e, _) => &e.attrs,
        }
    }

    pub fn id(&self) -> NodeId {
        match *self {
            StmtDecl(_, id) => id,
            StmtExpr(_, id) => id,
            StmtSemi(_, id) => id,
        }
    }
}

// FIXME (pending discussion of #1697, #2178...): local should really be
// a refinement on pat.
/// Local represents a `let` statement, e.g., `let <pat>:<ty> = <expr>;`
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct Local {
    pub pat: P<Pat>,
    pub ty: Option<P<Ty>>,
    /// Initializer expression to set the value, if any
    pub init: Option<P<Expr>>,
    pub id: NodeId,
    pub span: Span,
    pub attrs: ThinVec<Attribute>,
}

pub type Decl = Spanned<Decl_>;

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub enum Decl_ {
    /// A local (let) binding:
    DeclLocal(P<Local>),
    /// An item binding:
    DeclItem(ItemId),
}

impl Decl_ {
    pub fn attrs(&self) -> &[Attribute] {
        match *self {
            DeclLocal(ref l) => &l.attrs,
            DeclItem(_) => &[]
        }
    }
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct Field {
    pub name: Spanned<Name>,
    pub expr: P<Expr>,
    pub span: Span,
    pub is_shorthand: bool,
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug, Copy)]
pub enum BlockCheckMode {
    DefaultBlock,
    UnsafeBlock(UnsafeSource),
    PushUnsafeBlock(UnsafeSource),
    PopUnsafeBlock(UnsafeSource),
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug, Copy)]
pub enum UnsafeSource {
    CompilerGenerated,
    UserProvided,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct BodyId {
    pub node_id: NodeId,
}

/// The body of a function or constant value.
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct Body {
    pub arguments: HirVec<Arg>,
    pub value: Expr
}

impl Body {
    pub fn id(&self) -> BodyId {
        BodyId {
            node_id: self.value.id
        }
    }
}

/// An expression
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash)]
pub struct Expr {
    pub id: NodeId,
    pub span: Span,
    pub node: Expr_,
    pub attrs: ThinVec<Attribute>,
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "expr({}: {})", self.id,
               print::to_string(print::NO_ANN, |s| s.print_expr(self)))
    }
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub enum Expr_ {
    /// An array (`[a, b, c, d]`)
    ExprArray(HirVec<Expr>),
    /// A function call
    ///
    /// The first field resolves to the function itself (usually an `ExprPath`),
    /// and the second field is the list of arguments
    ExprCall(P<Expr>, HirVec<Expr>),
    /// A method call (`x.foo::<Bar, Baz>(a, b, c, d)`)
    ///
    /// The `Spanned<Name>` is the identifier for the method name.
    /// The vector of `Ty`s are the ascripted type parameters for the method
    /// (within the angle brackets).
    ///
    /// The first element of the vector of `Expr`s is the expression that
    /// evaluates to the object on which the method is being called on (the
    /// receiver), and the remaining elements are the rest of the arguments.
    ///
    /// Thus, `x.foo::<Bar, Baz>(a, b, c, d)` is represented as
    /// `ExprMethodCall(foo, [Bar, Baz], [x, a, b, c, d])`.
    ExprMethodCall(Spanned<Name>, HirVec<P<Ty>>, HirVec<Expr>),
    /// A tuple (`(a, b, c ,d)`)
    ExprTup(HirVec<Expr>),
    /// A binary operation (For example: `a + b`, `a * b`)
    ExprBinary(BinOp, P<Expr>, P<Expr>),
    /// A unary operation (For example: `!x`, `*x`)
    ExprUnary(UnOp, P<Expr>),
    /// A literal (For example: `1`, `"foo"`)
    ExprLit(P<Lit>),
    /// A cast (`foo as f64`)
    ExprCast(P<Expr>, P<Ty>),
    ExprType(P<Expr>, P<Ty>),
    /// An `if` block, with an optional else block
    ///
    /// `if expr { block } else { expr }`
    ExprIf(P<Expr>, P<Block>, Option<P<Expr>>),
    /// A while loop, with an optional label
    ///
    /// `'label: while expr { block }`
    ExprWhile(P<Expr>, P<Block>, Option<Spanned<Name>>),
    /// Conditionless loop (can be exited with break, continue, or return)
    ///
    /// `'label: loop { block }`
    ExprLoop(P<Block>, Option<Spanned<Name>>, LoopSource),

    /// A block (`{ ... }`)
    ExprBlock(P<Block>),

    /// An assignment (`a = foo()`)
    ExprAssign(P<Expr>, P<Expr>),
    /// An assignment with an operator
    ///
    /// For example, `a += 1`.
    ExprAssignOp(BinOp, P<Expr>, P<Expr>),
    /// Access of a named struct field (`obj.foo`)
    ExprField(P<Expr>, Spanned<Name>),
    /// Access of an unnamed field of a struct or tuple-struct
    ///
    /// For example, `foo.0`.
    ExprTupField(P<Expr>, Spanned<usize>),
    /// An indexing operation (`foo[2]`)
    ExprIndex(P<Expr>, P<Expr>),

    /// Path to a definition, possibly containing lifetime or type parameters.
    ExprPath(QPath),

    /// A referencing operation (`&a` or `&mut a`)
    ExprAddrOf(Mutability, P<Expr>),
    /// A `break`, with an optional label to break
    ExprBreak(Option<Label>, Option<P<Expr>>),
    /// A `continue`, with an optional label
    ExprAgain(Option<Label>),
    /// A `return`, with an optional value to be returned
    ExprRet(Option<P<Expr>>),

    /// A struct or struct-like variant literal expression.
    ///
    /// For example, `Foo {x: 1, y: 2}`, or
    /// `Foo {x: 1, .. base}`, where `base` is the `Option<Expr>`.
    ExprStruct(QPath, HirVec<Field>, Option<P<Expr>>),

    /// An array literal constructed from one repeated element.
    ///
    /// For example, `[1; 5]`. The first expression is the element
    /// to be repeated; the second is the number of times to repeat it.
    ExprRepeat(P<Expr>, BodyId),
}

/// Optionally `Self`-qualified value/type path or associated extension.
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub enum QPath {
    /// Path to a definition, optionally "fully-qualified" with a `Self`
    /// type, if the path points to an associated item in a trait.
    ///
    /// E.g. an unqualified path like `Clone::clone` has `None` for `Self`,
    /// while `<Vec<T> as Clone>::clone` has `Some(Vec<T>)` for `Self`,
    /// even though they both have the same two-segment `Clone::clone` `Path`.
    Resolved(Option<P<Ty>>, P<Path>),

    /// Type-related paths, e.g. `<T>::default` or `<T>::Output`.
    /// Will be resolved by type-checking to an associated item.
    ///
    /// UFCS source paths can desugar into this, with `Vec::new` turning into
    /// `<Vec>::new`, and `T::X::Y::method` into `<<<T>::X>::Y>::method`,
    /// the `X` and `Y` nodes being each a `TyPath(QPath::TypeRelative(..))`.
    TypeRelative(P<Ty>, P<PathSegment>)
}

/// The loop type that yielded an ExprLoop
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug, Copy)]
pub enum LoopSource {
    /// A `loop { .. }` loop
    Loop,
    /// A `while let _ = _ { .. }` loop
    WhileLet,
    /// A `for _ in _ { .. }` loop
    ForLoop,
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug, Copy)]
pub struct Label {
    pub span: Span,
    pub name: Name,
    pub loop_id: NodeId
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash)]
pub struct Ty {
    pub id: NodeId,
    pub node: Ty_,
    pub span: Span,
}

impl fmt::Debug for Ty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "type({})",
               print::to_string(print::NO_ANN, |s| s.print_type(self)))
    }
}

/// Not represented directly in the AST, referred to by name through a ty_path.
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug, Copy)]
pub enum PrimTy {
    TyInt(IntTy),
    TyUint(UintTy),
    TyFloat(FloatTy),
    TyStr,
    TyBool,
    TyChar,
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct BareFnTy {
    pub unsafety: Unsafety,
    pub abi: Abi,
    pub decl: P<FnDecl>,
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub enum Ty_ {
    /// A path to a type definition (`module::module::...::Type`), or an
    /// associated type, e.g. `<Vec<T> as Trait>::Type` or `<T>::Target`.
    ///
    /// Type parameters may be stored in each `PathSegment`.
    TyPath(QPath),
    /// A bare function (e.g. `fn(usize) -> bool`)
    TyBareFn(P<BareFnTy>),
}

/// represents an argument in a function header
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct Arg {
    pub pat: P<Pat>,
    pub id: NodeId,
}

/// Represents the header (not the body) of a function declaration
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct FnDecl {
    pub inputs: HirVec<P<Ty>>,
    pub output: FunctionRetTy,
    pub variadic: bool,
}

#[derive(Copy, Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub enum Unsafety {
    Unsafe,
    Normal,
}

#[derive(Copy, Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub enum Constness {
    Const,
    NotConst,
}

impl fmt::Display for Unsafety {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(match *self {
                              Unsafety::Normal => "normal",
                              Unsafety::Unsafe => "unsafe",
                          },
                          f)
    }
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub enum FunctionRetTy {
    /// Return type is not specified.
    ///
    /// Functions default to `()` and
    /// closures default to inference. Span points to where return
    /// type would be inserted.
    DefaultReturn(Span),
    /// Everything else
    Return(P<Ty>),
}

impl FunctionRetTy {
    pub fn span(&self) -> Span {
        match *self {
            DefaultReturn(span) => span,
            Return(ref ty) => ty.span,
        }
    }
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct Mod {
    /// A span from the first token past `{` to the last token until `}`.
    /// For `mod foo;`, the inner span ranges from the first token
    /// to the last token in the external file.
    pub inner: Span,
    pub item_ids: HirVec<ItemId>,
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct ForeignMod {
    pub abi: Abi,
    pub items: HirVec<ForeignItem>,
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct EnumDef {
    pub variants: HirVec<Variant>,
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct Variant_ {
    pub name: Name,
    pub attrs: HirVec<Attribute>,
    pub data: VariantData,
    /// Explicit discriminant, eg `Foo = 1`
    pub disr_expr: Option<BodyId>,
}

pub type Variant = Spanned<Variant_>;

#[derive(Copy, Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub enum UseKind {
    /// One import, e.g. `use foo::bar` or `use foo::bar as baz`.
    /// Also produced for each element of a list `use`, e.g.
    // `use foo::{a, b}` lowers to `use foo::a; use foo::b;`.
    Single,

    /// Glob import, e.g. `use foo::*`.
    Glob,

    /// Degenerate list import, e.g. `use foo::{a, b}` produces
    /// an additional `use foo::{}` for performing checks such as
    /// unstable feature gating. May be removed in the future.
    ListStem,
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub enum Visibility {
    Public,
    Crate,
    Restricted { path: P<Path>, id: NodeId },
    Inherited,
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct StructField {
    pub span: Span,
    pub name: Name,
    pub vis: Visibility,
    pub id: NodeId,
    pub ty: P<Ty>,
    pub attrs: HirVec<Attribute>,
}

impl StructField {
    // Still necessary in couple of places
    pub fn is_positional(&self) -> bool {
        let first = self.name.as_str().as_bytes()[0];
        first >= b'0' && first <= b'9'
    }
}

/// Fields and Ids of enum variants and structs
///
/// For enum variants: `NodeId` represents both an Id of the variant itself (relevant for all
/// variant kinds) and an Id of the variant's constructor (not relevant for `Struct`-variants).
/// One shared Id can be successfully used for these two purposes.
/// Id of the whole enum lives in `Item`.
///
/// For structs: `NodeId` represents an Id of the structure's constructor, so it is not actually
/// used for `Struct`-structs (but still presents). Structures don't have an analogue of "Id of
/// the variant itself" from enum variants.
/// Id of the whole struct lives in `Item`.
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub enum VariantData {
    Struct(HirVec<StructField>, NodeId),
    Tuple(HirVec<StructField>, NodeId),
    Unit(NodeId),
}

impl VariantData {
    pub fn fields(&self) -> &[StructField] {
        match *self {
            VariantData::Struct(ref fields, _) | VariantData::Tuple(ref fields, _) => fields,
            _ => &[],
        }
    }
    pub fn id(&self) -> NodeId {
        match *self {
            VariantData::Struct(_, id) | VariantData::Tuple(_, id) | VariantData::Unit(id) => id,
        }
    }
    pub fn is_struct(&self) -> bool {
        if let VariantData::Struct(..) = *self {
            true
        } else {
            false
        }
    }
    pub fn is_tuple(&self) -> bool {
        if let VariantData::Tuple(..) = *self {
            true
        } else {
            false
        }
    }
    pub fn is_unit(&self) -> bool {
        if let VariantData::Unit(..) = *self {
            true
        } else {
            false
        }
    }
}

// The bodies for items are stored "out of line", in a separate
// hashmap in the `Crate`. Here we just record the node-id of the item
// so it can fetched later.
#[derive(Copy, Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct ItemId {
    pub id: NodeId,
}

/// An item
///
/// The name might be a dummy name in case of anonymous items
#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct Item {
    pub name: Name,
    pub attrs: HirVec<Attribute>,
    pub id: NodeId,
    pub node: Item_,
    pub vis: Visibility,
    pub span: Span,
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub enum Item_ {
    /// An`extern crate` item, with optional original crate name,
    ///
    /// e.g. `extern crate foo` or `extern crate foo_bar as foo`
    ItemExternCrate(Option<Name>),

    /// `use foo::bar::*;` or `use foo::bar::baz as quux;`
    ///
    /// or just
    ///
    /// `use foo::bar::baz;` (with `as baz` implicitly on the right)
    ItemUse(P<Path>, UseKind),

    /// A `static` item
    ItemStatic(P<Ty>, Mutability, BodyId),
    /// A `const` item
    ItemConst(P<Ty>, BodyId),
    /// A function declaration
    ItemFn(P<FnDecl>, Unsafety, Constness, Abi, Generics, BodyId),
    /// A module
    ItemMod(Mod),
    /// An external module
    ItemForeignMod(ForeignMod),
    /// A type alias, e.g. `type Foo = Bar<u8>`
    ItemTy(P<Ty>, Generics),
    /// An enum definition, e.g. `enum Foo<A, B> {C<A>, D<B>}`
    ItemEnum(EnumDef, Generics),
    /// A struct definition, e.g. `struct Foo<A> {x: A}`
    ItemStruct(VariantData, Generics),
    /// A union definition, e.g. `union Foo<A, B> {x: A, y: B}`
    ItemUnion(VariantData, Generics),
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub struct ForeignItem {
    pub name: Name,
    pub attrs: HirVec<Attribute>,
    pub node: ForeignItem_,
    pub id: NodeId,
    pub span: Span,
    pub vis: Visibility,
}

#[derive(Clone, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub enum ForeignItem_ {
    /// A foreign function
    ForeignItemFn(P<FnDecl>, HirVec<Spanned<Name>>, Generics),
    /// A foreign static item (`static ext: u8`), with optional mutability
    /// (the boolean is true when mutable)
    ForeignItemStatic(P<Ty>, bool),
}

impl ForeignItem_ {
    pub fn descriptive_variant(&self) -> &str {
        match *self {
            ForeignItemFn(..) => "foreign function",
            ForeignItemStatic(..) => "foreign static item",
        }
    }
}