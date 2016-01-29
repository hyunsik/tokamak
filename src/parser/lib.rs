#![feature(convert)]
#[macro_use]
extern crate regex;

extern crate common;
extern crate plan;

pub mod parser;
pub mod lexer;

#[test]
fn test_tokenize() {
  use lexer::Token::*;
  let tokens = lexer::tokenize("1+-1.");
  assert_eq!(vec![Integer(1), Plus, Minus, Float(1.)], tokens);

  let tokens = lexer::tokenize("(1+4) * 3 + col1 / col2");
  assert_eq!(vec![LeftParen, Integer(1), Plus, Integer(4), RightParen, Multiply, Integer(3), Plus, Ident("col1".to_string()), Divide, Ident("col2".to_string())], tokens);

  let tokens = lexer::tokenize("'str1' == col1 and col2 <> 10 or col3 is not null or col4 == 'str2' + 3 / 5");
  assert_eq!(vec![LiteralStr("\'str1\'".to_string()), Eq, Ident("col1".to_string()), And, Ident("col2".to_string()), Ne, Integer(10), Or, Ident("col3".to_string()), IsNotNull, Or, Ident("col4".to_string()), Eq, LiteralStr("\'str2\'".to_string()), Plus, Integer(3), Divide, Integer(5)], tokens);
}

#[test]
fn test_parse_simple_expr() {
  use lexer::Token;
  use plan::expr;
  use plan::expr::*;
  use common::types::*;

  let tokens = lexer::tokenize("1+-1");
  assert_eq!(vec![Token::Integer(1), Token::Plus, Token::Minus, Token::Integer(1)], tokens);

  let mut ast = Vec::new();
  let parsed = parser::parse(tokens.as_slice(), ast.as_slice());
  assert_eq!(Ok((vec![expr::Add(&Ty::I64, expr::Const(1i64), expr::Const(-1i64))], vec![])), parsed);
}

#[test]
fn test_parse_complex_expr() {
  use lexer::Token;
  use plan::expr;
  use plan::expr::*;
  use common::types::*;

  let tokens = lexer::tokenize("(1+1) * (10-100) / 4.5 - 3");
  assert_eq!(vec![Token::LeftParen, Token::Integer(1), Token::Plus, Token::Integer(1), Token::RightParen,
    Token::Multiply,
    Token::LeftParen, Token::Integer(10), Token::Minus, Token::Integer(100), Token::RightParen,
    Token::Divide, Token::Float(4.5), Token::Minus, Token::Integer(3)],
    tokens);

  let mut ast = Vec::new();
  let parsed = parser::parse(tokens.as_slice(), ast.as_slice());
  assert_eq!(Ok((vec![
    expr::Sub(&Ty::F64,
    	expr::Div(&Ty::F64,
          expr::Mul(&Ty::I64,
            expr::Add(&Ty::I64,
          		expr::Const(1i64),
          		expr::Const(1i64)
            ),
            expr::Sub(&Ty::I64,
              expr::Const(10i64),
              expr::Const(100i64)
            )
          ),
          expr::Const(4.5f64)
        ),
        expr::Const(3i64)
    	)
  ], vec![])), parsed);
}
