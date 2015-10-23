use node::*;
use expr::*;

/// Visitor for Expr Tree
#[allow(unused_variables)]
pub trait Visitor<'v, T>: Sized {
 
  fn visit_relation(&self, ctx: &mut T, decl: &RelDecl) {}
  
  fn visit_partitioned_relation(&self, ctx: &mut T, decl: &PartitionedRelDecl) {}
  
  fn visit_join(&self, ctx: &mut T, left: &'v PlanNode, right: &'v PlanNode, decl: &JoinDecl) {
    walk_node(self, ctx, left);
    walk_node(self, ctx, right);
  }
  
  fn visit_project(&self, ctx: &mut T, child: &'v PlanNode, exprs: &Vec<Expr>) {
    walk_node(self, ctx, child);
  }
 
  fn visit_filter(&self, ctx: &mut T, child: &'v PlanNode, decl: &'v Vec<Expr>) {
    walk_node(self, ctx, child);
  }
 
  fn visit_aggregate(&self, ctx: &mut T, child: &'v PlanNode, decl: &'v &AggDecl) {
    walk_node(self, ctx, child);
  }
  
  fn visit_head(&self, ctx: &mut T, child: &'v PlanNode, rownum: usize) {
    walk_node(self, ctx, child);
  }
  
  fn visit_tail(&self, ctx: &mut T, child: &'v PlanNode, rownum: usize) {
    walk_node(self, ctx, child);
  }
}

/// Walker for Expr Tree
fn walk_node<'v, T, V>(v: &V, ctx: &mut T, node: &'v PlanNode) 
    where V: Visitor<'v, T> {
      
  match node.decl {

    NodeDecl::Relation (ref decl) => { 
      v.visit_relation(ctx, decl) 
    },
    
    NodeDecl::PartitionedRelation(ref decl) => { 
      v.visit_partitioned_relation(ctx, decl) 
    },
    
    NodeDecl::Join(ref left, ref right, ref decl) => { 
      v.visit_join(ctx, &**left, &**right, decl) 
    },
     
    NodeDecl::Project(ref child, ref exprs) => { 
      v.visit_project(ctx, &**child, exprs) 
    },
    
    NodeDecl::Filter(ref child, ref cond) => { 
      v.visit_filter(ctx, &**child, cond) 
    },
    
    NodeDecl::Head  (ref child, rownum) => { 
      v.visit_head(ctx, &**child, rownum) 
    },
    
    NodeDecl::Tail  (ref child, rownum) => { 
      v.visit_tail(ctx, &**child, rownum) 
    },
    
    _ => { panic!("Unknown type") },
  }
}