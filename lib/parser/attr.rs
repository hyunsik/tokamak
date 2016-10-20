// Functions dealing with attributes and meta items

use ast::{self, Attribute};

use codemap::Spanned;
use parser::{Parser, PResult, TokenType};

impl<'a> Parser<'a> {
  /// Parse attributes that appear after the opening of an item. These should
  /// be preceded by an exclamation mark, but we accept and warn about one
  /// terminated by a semicolon.

  /// matches inner_attrs*
  pub fn parse_inner_attributes(&mut self) -> PResult<'a, Vec<ast::Attribute>> {
    let mut attrs: Vec<ast::Attribute> = vec![];

    Ok(attrs)
  }
}