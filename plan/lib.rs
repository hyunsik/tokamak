//!
//! Plan
//!

extern crate common;
extern crate algebra;

use common::plugin::{TypeRegistry, FuncRegistry};

pub mod node;
use node::*;

pub mod expr;
mod expr_optimizer;

pub mod visitor;
use visitor::*;

mod planner;
pub use planner::*;

/*
fn typestr_to_schema(ctx: &PlanContext, types: &Vec<String>) -> Vec<Box<Type>>
{
  types.iter()
    .map( |s| ctx.type_registry().get(s).unwrap().clone_box() )
    .collect::<Vec<Box<Type>>>()
}
*/