use hir::def_id::DefId;

#[derive(Clone, Copy, PartialEq, Eq, RustcEncodable, RustcDecodable, Hash, Debug)]
pub enum Def {
  // Type namespace
  Mod(DefId),
  Struct(DefId), // DefId refers to NodeId of the struct itself

  // Value namespace
  Fn(DefId),
  Const(DefId),
  Static(DefId, bool /* is_mutbl */),
}