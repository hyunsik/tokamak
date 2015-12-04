use std::collections::HashSet;
use split::Split;

pub struct TaskSet;

pub struct TaskSetIterator;

impl Iterator for TaskSetIterator {
  type Item = Task;
  
  fn next(&mut self) -> Option<Task> {
    None
  }
}

pub struct Task;

impl IntoIterator for TaskSet {
  type Item = Task;
  type IntoIter = TaskSetIterator;
  
  fn into_iter(self) -> Self::IntoIter {
    TaskSetIterator
  }
}

pub struct TaskSetRefIterator<'a> 
{
  tasks: Option<&'a TaskSet>
}

impl<'a> Iterator for TaskSetRefIterator<'a> {
  type Item = &'a Task;
  
  fn next(&mut self) -> Option<&'a Task> {
    None
  }
}

impl<'a> IntoIterator for &'a TaskSet {
  type Item = &'a Task;
  type IntoIter = TaskSetRefIterator<'a>;
  
  fn into_iter(self) -> Self::IntoIter {
    TaskSetRefIterator { tasks: None }
  }
}

pub type PlanNodeId = String;

pub struct TaskSource {
  plan_node_id: PlanNodeId,
  splits      : HashSet<Split>
}