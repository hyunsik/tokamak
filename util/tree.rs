//! Generic Tree Implementation for Plan

pub enum TreeBuildError 
{
  EmptyStack,
  StillRemainStackItem
}

pub enum TreeNode<T> 
{
  Branch(T, Vec<Box<TreeNode<T>>>),
  Leaf(T)
}

#[allow(unused_variables)]
pub trait Visitor<'v, T>: Sized {
  fn visit(&self, data: &'v T, child: Option<&'v Vec<Box<TreeNode<T>>>>) {
    self.visit_by_default(child);
  }
  
  fn visit_by_default(&self, child: Option<&'v Vec<Box<TreeNode<T>>>>) {
    match child {
      Some(v) => {
        for node in v.iter().map(|n| &*n) {
          walk_tree(self, node);
        }
      },
      None    => {}
    };
  }
}

pub fn walk_tree<'v, V, T>(v: &V, node: &'v TreeNode<T>)
    where V: Visitor<'v, T> {
  match *node {
    TreeNode::Leaf(ref data) => v.visit(data, None),
    TreeNode::Branch(ref data, ref child) => v.visit(data, Some(child))
  };
}

/// Tree Builder in a bottom up approach.
pub struct TreeBuilder<T> 
{
  stack: Vec<TreeNode<T>>
}

impl<T> TreeBuilder<T> {
  
  pub fn new() -> TreeBuilder<T> 
  {
    TreeBuilder {
      stack: Vec::new()
    }
  }
  
  pub fn push(&mut self, node: TreeNode<T>) -> Result<usize, TreeBuildError>
  {
    self.stack.push(node);
    
    Ok(self.stack.len())
  }
  
  pub fn pop(&mut self) -> TreeNode<T> 
  {
    debug_assert!(self.stack.len() > 0);
    self.stack.pop().unwrap()
  }
  
  pub fn pop_and_push(&mut self, data: T, child_num: usize) -> Result<usize, TreeBuildError>
  {
    let stack_size = self.stack.len();
    debug_assert!(stack_size >= child_num);
    
    
    let child = self.stack.split_off(stack_size - child_num)
      .into_iter()
      .map(|node| Box::new(node))
      .collect::<Vec<Box<TreeNode<T>>>>();
       
    self.stack.push(TreeNode::Branch(data, child));
    
    Ok(self.stack.len())
  }
  
  pub fn build(&mut self) -> Result<TreeNode<T>, TreeBuildError>  {
    match self.stack.len() {
      0 => { Err(TreeBuildError::EmptyStack) },
      1 => { Ok(self.stack.pop().unwrap()) },
      _ => { Err(TreeBuildError::StillRemainStackItem) }
   }
  }
}

#[cfg(test)]
mod tests {
  use super::{TreeBuilder, TreeNode};

  
  
  #[test]
  fn test_tree() {
    let mut builder: TreeBuilder<&'static str> = TreeBuilder::new();
    
    builder.push(TreeNode::Leaf("l1")).ok();
    builder.push(TreeNode::Leaf("l2")).ok();
    
    builder.pop_and_push("l3", 2).ok();
    
    let tree = builder.build().ok().unwrap();
  }
}