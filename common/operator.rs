//! Operator Registry

use std::collections::BTreeMap;

//use super::datum::Datum;
use super::types::Ty;
use super::rows::{MiniPage,PageBuilder};

/// Expression Element
#[derive(Clone)]
pub struct Expr {
  //ty: Box<Type>,
  node: ExprSpec
}

/// Expression Specific Element
#[derive(Clone)]
pub enum ExprSpec {
  Op
}

/// Comparison Operator Type
#[derive(Clone, Copy)]
pub enum CompOp {
  Eq,
  Ne,
  Lt,
  Le,
  Gt,
  Ge
}

/// Arithmetic Operator Type
#[derive(Clone, Copy)]
pub enum ArithmOp {
  Plus,
  Sub,
  Mul,
  Div,
  Rem,
}

/// Function Declaration
#[derive(Clone)]
pub struct FnDecl {
  signature: String
}

/// Aggregation Function Declaration
#[derive(Clone)]
pub struct AggFnDecl {
  signature: String
}

/// Window Function Declaration
#[derive(Clone)]
pub struct WinFnDecl {
  signature: String
}


pub type UnaryFn   = Fn(&MiniPage, &mut PageBuilder);
pub type BinaryFn  = Fn(&MiniPage, &MiniPage, &mut PageBuilder);
pub type TrinityFn = Fn(&MiniPage, &MiniPage, &MiniPage, &mut PageBuilder);

pub enum InvokeAction
{
  Unary  (Box<UnaryFn>),
  Binary (Box<BinaryFn>),
  Trinity(Box<TrinityFn>)
}

pub struct OperatorSignature 
{
  name: String,
  params: Vec<Ty>
}

pub struct OperatorRegistry
{
  // key and value will be kept immutable as a just reference
  types: BTreeMap<OperatorSignature, InvokeAction>
}