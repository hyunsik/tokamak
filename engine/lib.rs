//!
//! Converting Phases
//!
//! # Simple Version
//!
//! A DataFrame dag --(ExecutorPlanner)--> A ExecutionPlan --(Parallelizer)--> A set of Tasks
//! --(TaskRunner)--> DataSet

extern crate common;
extern crate algebra;
extern crate exec;
extern crate optimizer;
extern crate plan;
extern crate rows;
extern crate storage;

mod runner;
pub use runner::{
  MaterializedResult,
  QueryExecutor 
};

mod local_runner;
pub use local_runner::LocalQueryExecutor;