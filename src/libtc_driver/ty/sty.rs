// NB: If you change this, you'll probably want to change the corresponding
// AST structure in libsyntax/ast.rs as well.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum TypeVariants<'tcx> {
  /// The primitive boolean type. Written as `bool`.
  TyBool,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum AdtKind { Struct, Enum }

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, RustcEncodable, RustcDecodable)]
pub enum VariantKind { Struct, Tuple, Unit }