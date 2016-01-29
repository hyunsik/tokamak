use regex::Regex;

use self::Token::{
  Delimiter,
  LeftParen,
  RightParen,
  Comma,
  Plus,
  Minus,
  Multiply,
  Divide,
  Modulus,
  Not,
  IsNull,
  IsNotNull,
  And,
  Or,
  Eq,
  Ne,
  Lt,
  Le,
  Gt,
  Ge,
  Ident,  // function call, field name,
  Float,
  Integer,
  LiteralStr,
};

#[derive(PartialEq, Clone, Debug)]
pub enum Token {
  Fn, // function definition
  Extern, // external function definition
  Switch,
  Case,
  If,
  Else,
  For,
  In, // TODO until here
  Delimiter,
  LeftParen,
  RightParen,
  Comma,
  Plus,
  Minus,
  Multiply,
  Divide,
  Modulus,
  Not,
  IsNull,
  IsNotNull,
  And,
  Or,
  Eq,
  Ne,
  Lt,
  Le,
  Gt,
  Ge,
  Ident(String),  // function call, field name,
  Float(f64),
  Integer(i64),
  LiteralStr(String),
  // TODO
  // cast
}

pub fn tokenize(input: &str) -> Vec<Token> {
  let comment_re = Regex::new(r"(?m)#.*\n").unwrap();  // comments are start with '#'.
  let preprocessed = comment_re.replace_all(input, "\n");

  let token_re = Regex::new(concat!(
    r"(?P<and>and)|",
    r"(?P<or>or)|",
    r"(?P<is_null>is\snull)|",
    r"(?P<is_not_null>is\snot\snull)|",
    r"(?P<not>not)|",
    r"(?P<literal_str>'\p{Alphabetic}\w*')|",
    r"(?P<ident>\p{Alphabetic}\w*)|",
    r"(?P<float>\d+\.\d*)|",
    r"(?P<integer>\d+)|",
    r"(?P<delim>;)|",
    r"(?P<lpar>\()|",
    r"(?P<rpar>\))|",
    r"(?P<comma>,)|",
    r"(?P<plus>\+)|",
    r"(?P<minus>-)|",
    r"(?P<multiply>\*)|",
    r"(?P<divide>/)|",
    r"(?P<modulus>%)|",
    r"(?P<eq>==)|",
    r"(?P<ne><>)|",
    r"(?P<lt><)|",
    r"(?P<le><=)|",
    r"(?P<gt>>)|",
    r"(?P<ge>>=)",
  )).unwrap();

  let types = vec!["and", "or", "is_null", "is_not_null", "not", "literal_str", "ident", "float", "integer",
    "delim", "lpar", "rpar", "comma", "plus", "minus", "multiply", "divide", "modulus", "eq",
    "ne", "lt", "le", "gt", "ge"];

  let result: Vec<Token> = token_re.captures_iter(preprocessed.as_str()).map(|cap| {
    match types.iter().find(|keyword| cap.name(keyword).is_some()) {
        None => panic!(format!("Undefined token")),
        Some(k) => {
          match *k {
            "and" => And,
            "or" => Or,
            "is_null" => IsNull,
            "is_not_null" => IsNotNull,
            "not" => Not,
            "literal_str" => LiteralStr(cap.name("literal_str").unwrap().to_string()),
            "ident" => Ident(cap.name("ident").unwrap().to_string()),
            "float" => {
              match cap.name("float") {
                Some(val) => match val.parse() {
                  Ok(float) => Float(float),
                  Err(_) => panic!(format!("Lexer failed trying to parse "))
                },
                None => panic!(format!("Lexer failed trying to parse "))
              }
            },
            "integer" => {
              match cap.name("integer") {
                Some(val) => match val.parse() {
                  Ok(integer) => Integer(integer),
                  Err(_) => panic!(format!("Lexer failed trying to parse "))
                },
                None => panic!(format!("Lexer failed trying to parse "))
              }
            }
            "delimiter" => Delimiter,
            "lpar" => LeftParen,
            "rpar" => RightParen,
            "comma" => Comma,
            "plus" => Plus,
            "minus" => Minus,
            "multiply" => Multiply,
            "divide" => Divide,
            "modulus" => Modulus,
            "eq" => Eq,
            "ne" => Ne,
            "lt" => Lt,
            "le" => Le,
            "gt" => Gt,
            "ge" => Ge,
            _ => panic!("Undefined token: {}", *k)
          }
        }
      }
  }).collect::<Vec<_>>();

  result
}
