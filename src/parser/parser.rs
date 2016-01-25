use lexer::Token;

use common::types::Ty;
use plan::expr;
use plan::expr::*;

use self::PartParsingResult::{Done, NotComplete, Error};

/// Associative operator with precedence.
///
/// This is the enum which specifies operator precedence and fixity to the parser.
#[derive(Debug, PartialEq, Eq)]
pub enum AssocOp {
    /// `+`
    Add,
    /// `-`
    Subtract,
    /// `*`
    Multiply,
    /// `/`
    Divide,
    /// `%`
    Modulus,
    /// `&`
    And,
    /// `|`
    Or,
    /// `==`
    Equal,
    /// `<`
    Less,
    /// `<=`
    LessEqual,
    /// `!=`
    NotEqual,
    /// `>`
    Greater,
    /// `>=`
    GreaterEqual,
    /// `=`
    Assign,
}

impl AssocOp {
  fn from_token(t: &Token) -> Option<AssocOp> {
    match *t {
      Token::Multiply => Some(AssocOp::Multiply),
      Token::Divide => Some(AssocOp::Divide),
      Token::Modulus => Some(AssocOp::Modulus),
      Token::Plus => Some(AssocOp::Add),
      Token::Minus => Some(AssocOp::Subtract),
      Token::Lt => Some(AssocOp::Less),
      Token::Le => Some(AssocOp::LessEqual),
      Token::Ge => Some(AssocOp::GreaterEqual),
      Token::Gt => Some(AssocOp::Greater),
      Token::Eq => Some(AssocOp::Equal),
      Token::Ne => Some(AssocOp::NotEqual),
      Token::And => Some(AssocOp::And),
      Token::Or => Some(AssocOp::Or),
      _ => None
    }
  }

  fn to_expr(t: &Token, lhs: &Expr, rhs: &Expr) -> Option<Expr> {
    match *t {
      Token::Multiply => Some(expr::Mul(ArithmOp::out_type(lhs.ty(), rhs.ty()), lhs.clone(), rhs.clone())),
      Token::Divide => Some(expr::Div(ArithmOp::out_type(lhs.ty(), rhs.ty()), lhs.clone(), rhs.clone())),
      Token::Modulus => Some(expr::Modulus(ArithmOp::out_type(lhs.ty(), rhs.ty()), lhs.clone(), rhs.clone())),
      Token::Plus => Some(expr::Add(ArithmOp::out_type(lhs.ty(), rhs.ty()), lhs.clone(), rhs.clone())),
      Token::Minus => Some(expr::Sub(ArithmOp::out_type(lhs.ty(), rhs.ty()), lhs.clone(), rhs.clone())),
      Token::Lt => Some(expr::Cmp(CmpOp::Lt, lhs.clone(), rhs.clone())),
      Token::Le => Some(expr::Cmp(CmpOp::Le, lhs.clone(), rhs.clone())),
      Token::Ge => Some(expr::Cmp(CmpOp::Ge, lhs.clone(), rhs.clone())),
      Token::Gt => Some(expr::Cmp(CmpOp::Gt, lhs.clone(), rhs.clone())),
      Token::Eq => Some(expr::Cmp(CmpOp::Eq, lhs.clone(), rhs.clone())),
      Token::Ne => Some(expr::Cmp(CmpOp::Ne, lhs.clone(), rhs.clone())),
      Token::And => Some(expr::And(lhs.clone(), rhs.clone())),
      Token::Or => Some(expr::Or(lhs.clone(), rhs.clone())),
      _ => None
    }
  }

  fn precedence(&self) -> usize {
    use self::AssocOp::*;
      match *self {
        Multiply | Divide | Modulus => 13,
        Add | Subtract => 12,
        And => 10,
        Or => 8,
        Less | Greater | LessEqual | GreaterEqual | Equal | NotEqual => 7,
        _ => 0,
    }
  }
}

enum PartParsingResult<T> {
  Done(T, Vec<Token>),
  NotComplete,
  Error(String)
}

pub type ParsingResult = Result<(Vec<Expr>, Vec<Token>), String>;

macro_rules! parse_try(
  ($function: ident, $tokens: ident, $parsed_tokens: ident) => (
    parse_try!($function, $tokens, $parsed_tokens, )
  );

  ($function: ident, $tokens: ident, $parsed_tokens: ident, $($arg: expr),*) => (
    match $function($tokens, $($arg),*) {
      Done(ast, toks) => {
        $parsed_tokens.extend(toks.into_iter());
        ast
      },
      NotComplete => {
        $parsed_tokens.reverse();
        $tokens.extend($parsed_tokens.into_iter());
        return NotComplete
      },
      Error(message) => return Error(message)
    }
  )
);

macro_rules! expect_tokens(
  ([ $($token: pat, $value: expr, $result: stmt);+ ] <= $tokens: ident, $parsed_tokens: ident, $error: expr) => (
    match $tokens.pop() {
      $(
        Some($token) => {
          $parsed_tokens.push($value);
          $result
        },
      )+
      None => {
        $parsed_tokens.reverse();
        $tokens.extend($parsed_tokens.into_iter());
        return NotComplete
      },
      _ => return error($error)
    }
  );

  ([ $($token:pat, $value:expr, $result: stmt);+ ] else $not_matched: block <= $tokens: ident, $parsed_tokens: ident) => (
    match $tokens.last().map(|t| t.clone()) {
      $(
        Some($token) => {
          $tokens.pop();
          $parsed_tokens.push($value);
          $result
        },
      )+
      _ => {$not_matched}
    }
  );
);

fn error<T>(message: &str) -> PartParsingResult<T> {
  Error(message.to_string())
}

pub fn parse(tokens: &[Token], parsed_trees: &[Expr]) -> ParsingResult {
  let mut rest = tokens.to_vec();
  rest.reverse(); // to use like a stack

  let mut asts = parsed_trees.to_vec();

  loop {
    let cur_token = match rest.last() {
      Some(t) => t.clone(),
      None => break
    };

    let result = match cur_token {
      // Fn => parse_function(&mut rest), TODO
      // Extern => parse_extern(&mut rest), TODO
      Token::Delimiter => {
        rest.pop(); continue
      },
      _ => parse_expr(&mut rest),
    };

    match result {
      Done(ast_node, _) => asts.push(ast_node),
      NotComplete => break,
      Error(message) => return Err(message),
    }
  }

  rest.reverse();
  Ok((asts, rest))
}

fn parse_expr(tokens: &mut Vec<Token>) -> PartParsingResult<Expr> {
  let mut parsed_tokens = Vec::new();
  let lhs = parse_try!(parse_primary_expr, tokens, parsed_tokens);
  let expr = parse_try!(parse_binary_expr, tokens, parsed_tokens, 0, &lhs);
  Done(expr, parsed_tokens)
}

fn parse_primary_expr(tokens: &mut Vec<Token>) -> PartParsingResult<Expr> {
  match tokens.last() {
    Some(&Token::Plus) => parse_plus_expr(tokens),
    Some(&Token::Minus) => parse_minus_expr(tokens),
    Some(&Token::Ident(_)) => parse_ident_expr(tokens),
    Some(&Token::Float(_)) => parse_float_expr(tokens, false),
    Some(&Token::Integer(_)) => parse_integer_expr(tokens, false),
    Some(&Token::LiteralStr(_)) => parse_literal_str_expr(tokens),
    Some(&Token::LeftParen) => parse_paren_expr(tokens),
    None => NotComplete,
    _ => error("unknown token when expecting an expression")
  }
}

fn parse_plus_expr(tokens: &mut Vec<Token>) -> PartParsingResult<Expr> {
  tokens.pop();
  parse_primary_expr(tokens)
}

fn parse_minus_expr(tokens: &mut Vec<Token>) -> PartParsingResult<Expr> {
  tokens.pop();
  match tokens.last() {
    Some(&Token::Float(_)) => parse_float_expr(tokens, true),
    Some(&Token::Integer(_)) => parse_integer_expr(tokens, true),
    _ => error("unknown token when expecting an expression")
  }
}

fn parse_ident_expr(tokens: &mut Vec<Token>) -> PartParsingResult<Expr> {
  Error("Not supported yet".to_string())
}

fn parse_float_expr(tokens: &mut Vec<Token>, negative: bool) -> PartParsingResult<Expr> {
  let mut parsed_tokens = Vec::new();

  let mut value = expect_tokens!(
    [Token::Float(val), Token::Float(val), val] <= tokens, parsed_tokens, "float expected"
  );

  if negative {
    value *= -1.;
  }

  Done(Const(Literal::F64(value)), parsed_tokens)
}

fn parse_integer_expr(tokens: &mut Vec<Token>, negative: bool) -> PartParsingResult<Expr> {
  let mut parsed_tokens = Vec::new();

  let mut value = expect_tokens!(
    [Token::Integer(val), Token::Integer(val), val] <= tokens, parsed_tokens, "integer expected"
  );

  if negative {
    value *= -1;
  }

  Done(Const(Literal::I64(value)), parsed_tokens)
}

fn parse_literal_str_expr(tokens: &mut Vec<Token>) -> PartParsingResult<Expr> {
  Error("Not supported yet".to_string())
}

fn parse_paren_expr(tokens: &mut Vec<Token>) -> PartParsingResult<Expr> {
  tokens.pop(); // consume a left paren
  let mut parsed_tokens = vec![Token::LeftParen];

  let expr = parse_try!(parse_expr, tokens, parsed_tokens);

  let paren = expect_tokens!(
    [Token::RightParen, Token::RightParen, ()] <= tokens, parsed_tokens, "')' expected"
  );

  Done(expr, parsed_tokens)
}

fn parse_binary_expr(tokens: &mut Vec<Token>, op_preced: usize, lhs: &Expr) -> PartParsingResult<Expr> {
  let mut result = lhs.clone();
  let mut parsed_tokens = Vec::new();

  loop {
    let (op, preced) = match tokens.last() {
      Some(token) => match AssocOp::from_token(token) {
        Some(assoc_op) => {
          if assoc_op.precedence() >= op_preced {
            (token.clone(), assoc_op.precedence())
          } else {
            break
          }
        },
        None => break, // TODO: check
      },
      _ => break
    };

    tokens.pop();
    parsed_tokens.push(op.clone());

    let mut rhs = parse_try!(parse_primary_expr, tokens, parsed_tokens);

    loop {
      let binary_rhs = match tokens.last().map(|next_op| next_op.clone()) {
        Some(token) => match AssocOp::from_token(&token) {
          Some(assoc_op) => {
            if assoc_op.precedence() > preced {
              parse_try!(parse_binary_expr, tokens, parsed_tokens, assoc_op.precedence(), &rhs)
            } else {
              break
            }
          },
          None => break, // TODO: check
        },
        _ => break
      };

      rhs = binary_rhs;
    }

    result = match AssocOp::to_expr(&op, &result, &rhs) {
      Some(expr) => expr,
      None => panic!(format!("cannot convert to expr: {:?}", op))
    };
  }

  Done(result, parsed_tokens)
}
