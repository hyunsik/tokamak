//! Generic Tree Implementation for Plan

pub enum TreeBuildError {
  EmptyStack,
  StillRemainStackItem
}

pub enum TreeNode<T> {
  Branch(T, Vec<Box<TreeNode<T>>>),
  Leaf(T)
}

/// Tree Builder in a bottom up approach.
pub struct TreeBuilder<T> 
{
  stack: Vec<TreeNode<T>>
}

impl<T> TreeBuilder<T> {
  pub fn new() -> TreeBuilder<T> {
    TreeBuilder {
      stack: Vec::new()
    }
  }
  
  pub fn push(&mut self, node: TreeNode<T>) -> usize {
    self.stack.push(node);
    self.stack.len()
  }
  
  pub fn pop(&mut self) -> TreeNode<T> {
    debug_assert!(self.stack.len() > 0);
    
    self.stack.pop().unwrap()
  }
  
  pub fn build(&mut self) -> Result<TreeNode<T>, TreeBuildError>  {
    match self.stack.len() {
      0 => { Err(TreeBuildError::EmptyStack) },
      1 => { Ok(self.stack.pop().unwrap()) },
      _ => { Err(TreeBuildError::StillRemainStackItem) }
   }
  }
}