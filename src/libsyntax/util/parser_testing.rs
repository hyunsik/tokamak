use ast;
use parse::{ParseSess,PResult,filemap_to_tts};
use parse::new_parser_from_source_str;
use parse::parser::Parser;
use parse::token;
use ptr::P;
use str::char_at;

/// Map a string to tts, using a made-up filename:
pub fn string_to_tts(source_str: String) -> Vec<ast::TokenTree> {
    let ps = ParseSess::new();
    filemap_to_tts(&ps, ps.codemap().new_filemap("bogofile".to_string(), source_str))
}

/// Map string to parser (via tts)
pub fn string_to_parser<'a>(ps: &'a ParseSess, source_str: String) -> Parser<'a> {
    new_parser_from_source_str(ps,
                               Vec::new(),
                               "bogofile".to_string(),
                               source_str)
}

fn with_error_checking_parse<'a, T, F>(s: String, ps: &'a ParseSess, f: F) -> T where
    F: FnOnce(&mut Parser<'a>) -> PResult<'a, T>,
{
    let mut p = string_to_parser(&ps, s);
    let x = panictry!(f(&mut p));
    p.abort_if_errors();
    x
}

/// Parse a string, return a crate.
pub fn string_to_crate (source_str : String) -> ast::Crate {
    let ps = ParseSess::new();
    with_error_checking_parse(source_str, &ps, |p| {
        p.parse_crate_mod()
    })
}

/// Parse a string, return an expr
pub fn string_to_expr (source_str : String) -> P<ast::Expr> {
    let ps = ParseSess::new();
    with_error_checking_parse(source_str, &ps, |p| {
        p.parse_expr()
    })
}

/// Parse a string, return an item
pub fn string_to_item (source_str : String) -> Option<P<ast::Item>> {
    let ps = ParseSess::new();
    with_error_checking_parse(source_str, &ps, |p| {
        p.parse_item()
    })
}

/// Parse a string, return a stmt
pub fn string_to_stmt(source_str : String) -> Option<ast::Stmt> {
    let ps = ParseSess::new();
    with_error_checking_parse(source_str, &ps, |p| {
        p.parse_stmt()
    })
}

/// Parse a string, return a pat. Uses "irrefutable"... which doesn't
/// (currently) affect parsing.
pub fn string_to_pat(source_str: String) -> P<ast::Pat> {
    let ps = ParseSess::new();
    with_error_checking_parse(source_str, &ps, |p| {
        p.parse_pat()
    })
}

/// Convert a vector of strings to a vector of ast::Ident's
pub fn strs_to_idents(ids: Vec<&str> ) -> Vec<ast::Ident> {
    ids.iter().map(|u| token::str_to_ident(*u)).collect()
}

/// Given a string and an index, return the first usize >= idx
/// that is a non-ws-char or is outside of the legal range of
/// the string.
fn scan_for_non_ws_or_end(a : &str, idx: usize) -> usize {
    let mut i = idx;
    let len = a.len();
    while (i < len) && (is_whitespace(char_at(a, i))) {
        i += 1;
    }
    i
}

/// Copied from lexer.
pub fn is_whitespace(c: char) -> bool {
    return c == ' ' || c == '\t' || c == '\r' || c == '\n';
}