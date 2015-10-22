use expr::Expr;

pub struct PlanNode {
  pub decl: NodeDecl
}

pub enum NodeDecl {
  Relation           (RelDecl),  
  PartitionedRelation(PartitionedRelDecl),
  DerivedRelation    (DerivedRelDecl),
  Join      (Box<PlanNode>, Box<PlanNode>, JoinDecl),
  Project   (Box<PlanNode>, Vec<Expr>), // child and exprs
  Filter    (Box<PlanNode>, Vec<Expr>), // child and bool exprs
  Aggregate (Box<PlanNode>, AggDecl), // child and decl
  Head      (Box<PlanNode>, usize),   // child and row number
  Tail      (Box<PlanNode>, usize),   // child and row number
}

/// Table description
pub struct RelDecl;

/// Partitioned Table description
pub struct PartitionedRelDecl;

pub struct DerivedRelDecl
{
  block_id: u32,
  exprs   : Option<Vec<Expr>>
}

pub struct JoinDecl 
{
  cond  : Option<Vec<Expr>>,
  filter: Option<Vec<Expr>>
}

pub struct AggDecl {
  keys: Vec<Expr>, 
  aggrs: Vec<Expr>
}