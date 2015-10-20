//!
//! Converting Phases
//!
//! # Simple Version
//!
//! A DataFrame dag --(ExecutorPlanner)--> A ExecutionPlan --(Parallelizer)--> A set of Tasks
//! --(TaskRunner)--> DataSet

extern crate common;
extern crate exec;
extern crate rows;
extern crate storage;

pub struct ExecutorRunner;

mod planner;
pub use planner::*;

mod parallelizer;
pub use parallelizer::*;

mod optimizer;
pub use optimizer::*;

mod task;
pub use task::Task;

mod query_executor;
pub use query_executor::{
  MaterializedResult,
  LocalQueryExecutor,
  QueryExecutor 
};