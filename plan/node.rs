use expr::Expr;

#[derive(Clone)]
pub struct PlanNode {
  pub id  : u32,
  pub decl: NodeDecl
}

#[derive(Clone)]
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
#[derive(Clone)]
pub struct RelDecl;

/// Partitioned Table description
#[derive(Clone)]
pub struct PartitionedRelDecl;

#[derive(Clone)]
pub struct DerivedRelDecl
{
  block_id: u32,
  exprs   : Option<Vec<Expr>>
}

#[derive(Clone)]
pub struct JoinDecl 
{
  cond  : Option<Vec<Expr>>,
  filter: Option<Vec<Expr>>
}

#[derive(Clone)]
pub struct AggDecl {
  keys: Vec<Expr>, 
  aggrs: Vec<Expr>
}