//! The main parser interface

use ast;
use codemap::{self, Span, CodeMap, FileMap};
use errors::{Handler, ColorConfig, DiagnosticBuilder};
use parse::parser::Parser;
use parse::token::InternedString;
use ptr::P;
use str::char_at;

use std::cell::RefCell;
use std::io::Read;
use std::iter;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::str;

pub type PResult<'a, T> = Result<T, DiagnosticBuilder<'a>>;

#[macro_use]
pub mod parser;

pub mod lexer;
pub mod token;
pub mod attr;

pub mod common;
pub mod classify;
pub mod obsolete;


/// Info about a parsing session.
pub struct ParseSess {
  pub span_diagnostic: Handler, // better be the same as the one in the reader!
  /// Used to determine and report recursive mod inclusions
  included_mod_stack: RefCell<Vec<PathBuf>>,
  code_map: Rc<CodeMap>,
}

impl ParseSess {
    pub fn new() -> ParseSess {
        let cm = Rc::new(CodeMap::new());
        let handler = Handler::with_tty_emitter(ColorConfig::Auto, None, true, false, cm.clone());
        ParseSess::with_span_handler(handler, cm)
    }

    pub fn with_span_handler(handler: Handler, code_map: Rc<CodeMap>) -> ParseSess {
        ParseSess {
            span_diagnostic: handler,
            included_mod_stack: RefCell::new(vec![]),
            code_map: code_map
        }
    }

    pub fn codemap(&self) -> &CodeMap {
        &self.code_map
    }
}

// a bunch of utility functions of the form parse_<thing>_from_<source>
// where <thing> includes crate, expr, item, stmt, tts, and one that
// uses a HOF to parse anything, and <source> includes file and
// source_str.

pub fn parse_crate_from_file(
    input: &Path,
    cfg: ast::CrateConfig,
    sess: &ParseSess
) -> ast::Crate {
    let mut parser = new_parser_from_file(sess, cfg, input);
    abort_if_errors(parser.parse_crate_mod(), &parser)
}

pub fn parse_crate_attrs_from_file(
    input: &Path,
    cfg: ast::CrateConfig,
    sess: &ParseSess
) -> Vec<ast::Attribute> {
    let mut parser = new_parser_from_file(sess, cfg, input);
    abort_if_errors(parser.parse_inner_attributes(), &parser)
}

pub fn parse_crate_from_source_str(name: String,
                                   source: String,
                                   cfg: ast::CrateConfig,
                                   sess: &ParseSess)
                                   -> ast::Crate {
    let mut p = new_parser_from_source_str(sess,
                                           cfg,
                                           name,
                                           source);
    panictry!(p.parse_crate_mod())
}

pub fn parse_crate_attrs_from_source_str(name: String,
                                         source: String,
                                         cfg: ast::CrateConfig,
                                         sess: &ParseSess)
                                         -> Vec<ast::Attribute> {
    let mut p = new_parser_from_source_str(sess,
                                           cfg,
                                           name,
                                           source);
    panictry!(p.parse_inner_attributes())
}

pub fn parse_expr_from_source_str(name: String,
                                  source: String,
                                  cfg: ast::CrateConfig,
                                  sess: &ParseSess)
                                  -> P<ast::Expr> {
    let mut p = new_parser_from_source_str(sess, cfg, name, source);
    panictry!(p.parse_expr())
}

pub fn parse_item_from_source_str(name: String,
                                  source: String,
                                  cfg: ast::CrateConfig,
                                  sess: &ParseSess)
                                  -> Option<P<ast::Item>> {
    let mut p = new_parser_from_source_str(sess, cfg, name, source);
    panictry!(p.parse_item())
}

pub fn parse_meta_from_source_str(name: String,
                                  source: String,
                                  cfg: ast::CrateConfig,
                                  sess: &ParseSess)
                                  -> P<ast::MetaItem> {
    let mut p = new_parser_from_source_str(sess, cfg, name, source);
    panictry!(p.parse_meta_item())
}

pub fn parse_stmt_from_source_str(name: String,
                                  source: String,
                                  cfg: ast::CrateConfig,
                                  sess: &ParseSess)
                                  -> Option<ast::Stmt> {
    let mut p = new_parser_from_source_str(
        sess,
        cfg,
        name,
        source
    );
    panictry!(p.parse_stmt())
}

// Warning: This parses with quote_depth > 0, which is not the default.
pub fn parse_tts_from_source_str(name: String,
                                 source: String,
                                 cfg: ast::CrateConfig,
                                 sess: &ParseSess)
                                 -> Vec<ast::TokenTree> {
    let mut p = new_parser_from_source_str(
        sess,
        cfg,
        name,
        source
    );
    p.quote_depth += 1;
    // right now this is re-creating the token trees from ... token trees.
    panictry!(p.parse_all_token_trees())
}

// Create a new parser from a source string
pub fn new_parser_from_source_str<'a>(sess: &'a ParseSess,
                                      cfg: ast::CrateConfig,
                                      name: String,
                                      source: String)
                                      -> Parser<'a> {
    filemap_to_parser(sess, sess.codemap().new_filemap(name, source), cfg)
}

/// Create a new parser, handling errors as appropriate
/// if the file doesn't exist
pub fn new_parser_from_file<'a>(sess: &'a ParseSess,
                                cfg: ast::CrateConfig,
                                path: &Path) -> Parser<'a> {
    filemap_to_parser(sess, file_to_filemap(sess, path, None), cfg)
}

/// Given a session, a crate config, a path, and a span, add
/// the file at the given path to the codemap, and return a parser.
/// On an error, use the given span as the source of the problem.
pub fn new_sub_parser_from_file<'a>(sess: &'a ParseSess,
                                    cfg: ast::CrateConfig,
                                    path: &Path,
                                    owns_directory: bool,
                                    module_name: Option<String>,
                                    sp: Span) -> Parser<'a> {
    let mut p = filemap_to_parser(sess, file_to_filemap(sess, path, Some(sp)), cfg);
    p.owns_directory = owns_directory;
    p.root_module_name = module_name;
    p
}

/// Given a filemap and config, return a parser
pub fn filemap_to_parser<'a>(sess: &'a ParseSess,
                             filemap: Rc<FileMap>,
                             cfg: ast::CrateConfig) -> Parser<'a> {
    let end_pos = filemap.end_pos;
    let mut parser = tts_to_parser(sess, filemap_to_tts(sess, filemap), cfg);

    if parser.token == token::Eof && parser.span == codemap::DUMMY_SP {
        parser.span = codemap::mk_sp(end_pos, end_pos);
    }

    parser
}

// must preserve old name for now, because quote! from the *existing*
// compiler expands into it
pub fn new_parser_from_tts<'a>(sess: &'a ParseSess,
                               cfg: ast::CrateConfig,
                               tts: Vec<ast::TokenTree>) -> Parser<'a> {
    tts_to_parser(sess, tts, cfg)
}


// base abstractions

/// Given a session and a path and an optional span (for error reporting),
/// add the path to the session's codemap and return the new filemap.
fn file_to_filemap(sess: &ParseSess, path: &Path, spanopt: Option<Span>)
                   -> Rc<FileMap> {
    match sess.codemap().load_file(path) {
        Ok(filemap) => filemap,
        Err(e) => {
            let msg = format!("couldn't read {:?}: {}", path.display(), e);
            match spanopt {
                Some(sp) => panic!(sess.span_diagnostic.span_fatal(sp, &msg)),
                None => panic!(sess.span_diagnostic.fatal(&msg))
            }
        }
    }
}

/// Given a filemap, produce a sequence of token-trees
pub fn filemap_to_tts(sess: &ParseSess, filemap: Rc<FileMap>)
    -> Vec<ast::TokenTree> {
    // it appears to me that the cfg doesn't matter here... indeed,
    // parsing tt's probably shouldn't require a parser at all.
    let cfg = Vec::new();
    let srdr = lexer::StringReader::new(&sess.span_diagnostic, filemap);
    let mut p1 = Parser::new(sess, cfg, Box::new(srdr));
    panictry!(p1.parse_all_token_trees())
}

/// Given tts and produce a parser
pub fn tts_to_parser<'a>(sess: &'a ParseSess,
                         tts: Vec<ast::TokenTree>,
                         cfg: ast::CrateConfig) -> Parser<'a> {
    let trdr = lexer::new_tt_reader(&sess.span_diagnostic, None, tts);
    let mut p = Parser::new(sess, cfg, Box::new(trdr));
    p
}

fn abort_if_errors<'a, T>(result: PResult<'a, T>, p: &Parser) -> T {
    match result {
        Ok(c) => {
            c
        }
        Err(mut e) => {
            e.emit();
            p.abort_if_errors();
            unreachable!();
        }
    }
}

/// Parse a string representing a character literal into its final form.
/// Rather than just accepting/rejecting a given literal, unescapes it as
/// well. Can take any slice prefixed by a character escape. Returns the
/// character and the number of characters consumed.
pub fn char_lit(lit: &str) -> (char, isize) {
    use std::char;

    let mut chars = lit.chars();
    let c = match (chars.next(), chars.next()) {
        (Some(c), None) if c != '\\' => return (c, 1),
        (Some('\\'), Some(c)) => match c {
            '"' => Some('"'),
            'n' => Some('\n'),
            'r' => Some('\r'),
            't' => Some('\t'),
            '\\' => Some('\\'),
            '\'' => Some('\''),
            '0' => Some('\0'),
            _ => { None }
        },
        _ => panic!("lexer accepted invalid char escape `{}`", lit)
    };

    match c {
        Some(x) => return (x, 2),
        None => { }
    }

    let msg = format!("lexer should have rejected a bad character escape {}", lit);
    let msg2 = &msg[..];

    fn esc(len: usize, lit: &str) -> Option<(char, isize)> {
        u32::from_str_radix(&lit[2..len], 16).ok()
        .and_then(char::from_u32)
        .map(|x| (x, len as isize))
    }

    let unicode_escape = || -> Option<(char, isize)> {
        if lit.as_bytes()[2] == b'{' {
            let idx = lit.find('}').expect(msg2);
            let subslice = &lit[3..idx];
            u32::from_str_radix(subslice, 16).ok()
                .and_then(char::from_u32)
                .map(|x| (x, subslice.chars().count() as isize + 4))
        } else {
            esc(6, lit)
        }
    };

    // Unicode escapes
    return match lit.as_bytes()[1] as char {
        'x' | 'X' => esc(4, lit),
        'u' => unicode_escape(),
        'U' => esc(10, lit),
        _ => None,
    }.expect(msg2);
}

/// Parse a string representing a string literal into its final form. Does
/// unescaping.
pub fn str_lit(lit: &str) -> String {
    debug!("parse_str_lit: given {}", lit.escape_default());
    let mut res = String::with_capacity(lit.len());

    // FIXME #8372: This could be a for-loop if it didn't borrow the iterator
    let error = |i| format!("lexer should have rejected {} at {}", lit, i);

    /// Eat everything up to a non-whitespace
    fn eat<'a>(it: &mut iter::Peekable<str::CharIndices<'a>>) {
        loop {
            match it.peek().map(|x| x.1) {
                Some(' ') | Some('\n') | Some('\r') | Some('\t') => {
                    it.next();
                },
                _ => { break; }
            }
        }
    }

    let mut chars = lit.char_indices().peekable();
    loop {
        match chars.next() {
            Some((i, c)) => {
                match c {
                    '\\' => {
                        let ch = chars.peek().unwrap_or_else(|| {
                            panic!("{}", error(i))
                        }).1;

                        if ch == '\n' {
                            eat(&mut chars);
                        } else if ch == '\r' {
                            chars.next();
                            let ch = chars.peek().unwrap_or_else(|| {
                                panic!("{}", error(i))
                            }).1;

                            if ch != '\n' {
                                panic!("lexer accepted bare CR");
                            }
                            eat(&mut chars);
                        } else {
                            // otherwise, a normal escape
                            let (c, n) = char_lit(&lit[i..]);
                            for _ in 0..n - 1 { // we don't need to move past the first \
                                chars.next();
                            }
                            res.push(c);
                        }
                    },
                    '\r' => {
                        let ch = chars.peek().unwrap_or_else(|| {
                            panic!("{}", error(i))
                        }).1;

                        if ch != '\n' {
                            panic!("lexer accepted bare CR");
                        }
                        chars.next();
                        res.push('\n');
                    }
                    c => res.push(c),
                }
            },
            None => break
        }
    }

    res.shrink_to_fit(); // probably not going to do anything, unless there was an escape.
    debug!("parse_str_lit: returning {}", res);
    res
}

/// Parse a string representing a raw string literal into its final form. The
/// only operation this does is convert embedded CRLF into a single LF.
pub fn raw_str_lit(lit: &str) -> String {
    debug!("raw_str_lit: given {}", lit.escape_default());
    let mut res = String::with_capacity(lit.len());

    // FIXME #8372: This could be a for-loop if it didn't borrow the iterator
    let mut chars = lit.chars().peekable();
    loop {
        match chars.next() {
            Some(c) => {
                if c == '\r' {
                    if *chars.peek().unwrap() != '\n' {
                        panic!("lexer accepted bare CR");
                    }
                    chars.next();
                    res.push('\n');
                } else {
                    res.push(c);
                }
            },
            None => break
        }
    }

    res.shrink_to_fit();
    res
}

// check if `s` looks like i32 or u1234 etc.
fn looks_like_width_suffix(first_chars: &[char], s: &str) -> bool {
    s.len() > 1 &&
        first_chars.contains(&char_at(s, 0)) &&
        s[1..].chars().all(|c| '0' <= c && c <= '9')
}

fn filtered_float_lit(data: token::InternedString, suffix: Option<&str>,
                      sd: &Handler, sp: Span) -> ast::LitKind {
    debug!("filtered_float_lit: {}, {:?}", data, suffix);
    match suffix.as_ref().map(|s| &**s) {
        Some("f32") => ast::LitKind::Float(data, ast::FloatTy::F32),
        Some("f64") => ast::LitKind::Float(data, ast::FloatTy::F64),
        Some(suf) => {
            if suf.len() >= 2 && looks_like_width_suffix(&['f'], suf) {
                // if it looks like a width, lets try to be helpful.
                sd.struct_span_err(sp, &format!("invalid width `{}` for float literal", &suf[1..]))
                 .fileline_help(sp, "valid widths are 32 and 64")
                 .emit();
            } else {
                sd.struct_span_err(sp, &format!("invalid suffix `{}` for float literal", suf))
                  .fileline_help(sp, "valid suffixes are `f32` and `f64`")
                  .emit();
            }

            ast::LitKind::FloatUnsuffixed(data)
        }
        None => ast::LitKind::FloatUnsuffixed(data)
    }
}
pub fn float_lit(s: &str, suffix: Option<InternedString>,
                 sd: &Handler, sp: Span) -> ast::LitKind {
    debug!("float_lit: {:?}, {:?}", s, suffix);
    // FIXME #2252: bounds checking float literals is deferred until trans
    let s = s.chars().filter(|&c| c != '_').collect::<String>();
    let data = token::intern_and_get_ident(&s);
    filtered_float_lit(data, suffix.as_ref().map(|s| &**s), sd, sp)
}

/// Parse a string representing a byte literal into its final form. Similar to `char_lit`
pub fn byte_lit(lit: &str) -> (u8, usize) {
    let err = |i| format!("lexer accepted invalid byte literal {} step {}", lit, i);

    if lit.len() == 1 {
        (lit.as_bytes()[0], 1)
    } else {
        assert!(lit.as_bytes()[0] == b'\\', err(0));
        let b = match lit.as_bytes()[1] {
            b'"' => b'"',
            b'n' => b'\n',
            b'r' => b'\r',
            b't' => b'\t',
            b'\\' => b'\\',
            b'\'' => b'\'',
            b'0' => b'\0',
            _ => {
                match u64::from_str_radix(&lit[2..4], 16).ok() {
                    Some(c) =>
                        if c > 0xFF {
                            panic!(err(2))
                        } else {
                            return (c as u8, 4)
                        },
                    None => panic!(err(3))
                }
            }
        };
        return (b, 2);
    }
}

pub fn byte_str_lit(lit: &str) -> Rc<Vec<u8>> {
    let mut res = Vec::with_capacity(lit.len());

    // FIXME #8372: This could be a for-loop if it didn't borrow the iterator
    let error = |i| format!("lexer should have rejected {} at {}", lit, i);

    /// Eat everything up to a non-whitespace
    fn eat<'a, I: Iterator<Item=(usize, u8)>>(it: &mut iter::Peekable<I>) {
        loop {
            match it.peek().map(|x| x.1) {
                Some(b' ') | Some(b'\n') | Some(b'\r') | Some(b'\t') => {
                    it.next();
                },
                _ => { break; }
            }
        }
    }

    // byte string literals *must* be ASCII, but the escapes don't have to be
    let mut chars = lit.bytes().enumerate().peekable();
    loop {
        match chars.next() {
            Some((i, b'\\')) => {
                let em = error(i);
                match chars.peek().expect(&em).1 {
                    b'\n' => eat(&mut chars),
                    b'\r' => {
                        chars.next();
                        if chars.peek().expect(&em).1 != b'\n' {
                            panic!("lexer accepted bare CR");
                        }
                        eat(&mut chars);
                    }
                    _ => {
                        // otherwise, a normal escape
                        let (c, n) = byte_lit(&lit[i..]);
                        // we don't need to move past the first \
                        for _ in 0..n - 1 {
                            chars.next();
                        }
                        res.push(c);
                    }
                }
            },
            Some((i, b'\r')) => {
                let em = error(i);
                if chars.peek().expect(&em).1 != b'\n' {
                    panic!("lexer accepted bare CR");
                }
                chars.next();
                res.push(b'\n');
            }
            Some((_, c)) => res.push(c),
            None => break,
        }
    }

    Rc::new(res)
}

pub fn integer_lit(s: &str,
                   suffix: Option<InternedString>,
                   sd: &Handler,
                   sp: Span)
                   -> ast::LitKind {
    // s can only be ascii, byte indexing is fine

    let s2 = s.chars().filter(|&c| c != '_').collect::<String>();
    let mut s = &s2[..];

    debug!("integer_lit: {}, {:?}", s, suffix);

    let mut base = 10;
    let orig = s;
    let mut ty = ast::LitIntType::Unsuffixed;

    if char_at(s, 0) == '0' && s.len() > 1 {
        match char_at(s, 1) {
            'x' => base = 16,
            'o' => base = 8,
            'b' => base = 2,
            _ => { }
        }
    }

    // 1f64 and 2f32 etc. are valid float literals.
    if let Some(ref suf) = suffix {
        if looks_like_width_suffix(&['f'], suf) {
            match base {
                16 => sd.span_err(sp, "hexadecimal float literal is not supported"),
                8 => sd.span_err(sp, "octal float literal is not supported"),
                2 => sd.span_err(sp, "binary float literal is not supported"),
                _ => ()
            }
            let ident = token::intern_and_get_ident(&s);
            return filtered_float_lit(ident, Some(&suf), sd, sp)
        }
    }

    if base != 10 {
        s = &s[2..];
    }

    if let Some(ref suf) = suffix {
        if suf.is_empty() { sd.span_bug(sp, "found empty literal suffix in Some")}
        ty = match &**suf {
            "isize" => ast::LitIntType::Signed(ast::IntTy::Is),
            "i8"  => ast::LitIntType::Signed(ast::IntTy::I8),
            "i16" => ast::LitIntType::Signed(ast::IntTy::I16),
            "i32" => ast::LitIntType::Signed(ast::IntTy::I32),
            "i64" => ast::LitIntType::Signed(ast::IntTy::I64),
            "usize" => ast::LitIntType::Unsigned(ast::UintTy::Us),
            "u8"  => ast::LitIntType::Unsigned(ast::UintTy::U8),
            "u16" => ast::LitIntType::Unsigned(ast::UintTy::U16),
            "u32" => ast::LitIntType::Unsigned(ast::UintTy::U32),
            "u64" => ast::LitIntType::Unsigned(ast::UintTy::U64),
            _ => {
                // i<digits> and u<digits> look like widths, so lets
                // give an error message along those lines
                if looks_like_width_suffix(&['i', 'u'], suf) {
                    sd.struct_span_err(sp, &format!("invalid width `{}` for integer literal",
                                             &suf[1..]))
                      .fileline_help(sp, "valid widths are 8, 16, 32 and 64")
                      .emit();
                } else {
                    sd.struct_span_err(sp, &format!("invalid suffix `{}` for numeric literal", suf))
                      .fileline_help(sp, "the suffix must be one of the integral types \
                                      (`u32`, `isize`, etc)")
                      .emit();
                }

                ty
            }
        }
    }

    debug!("integer_lit: the type is {:?}, base {:?}, the new string is {:?}, the original \
           string was {:?}, the original suffix was {:?}", ty, base, s, orig, suffix);

    match u64::from_str_radix(s, base) {
        Ok(r) => ast::LitKind::Int(r, ty),
        Err(_) => {
            // small bases are lexed as if they were base 10, e.g, the string
            // might be `0b10201`. This will cause the conversion above to fail,
            // but these cases have errors in the lexer: we don't want to emit
            // two errors, and we especially don't want to emit this error since
            // it isn't necessarily true.
            let already_errored = base < 10 &&
                s.chars().any(|c| c.to_digit(10).map_or(false, |d| d >= base));

            if !already_errored {
                sd.span_err(sp, "int literal is too large");
            }
            ast::LitKind::Int(0, ty)
        }
    }
}

#[cfg(test)]
mod tests {
  use super::*;
  use ast::{self, TokenTree, Item};
  use parse::token::{Token, str_to_ident};
  use codemap::{Span, BytePos, Pos};
  use parse::lexer::{Reader, StringReader};
  use parse::parser::Parser;
  use util::parser_testing::{string_to_tts, string_to_parser};
  use util::parser_testing::{string_to_expr, string_to_item, string_to_stmt};

  use std::rc::Rc;

  // produce a codemap::span
  fn sp(a: u32, b: u32) -> Span {
    Span {lo: BytePos(a), hi: BytePos(b)}
  }

  #[test] fn items_1() {
    let x = string_to_item("fn xyz() { }".to_string());
  }

  #[test] fn items_2() {
    let x = string_to_item("fn xyz(x: i32) { }".to_string()).unwrap();
    println!("{:?}", x);
  }

  #[test] fn items_3() {
    let x = string_to_item("fn xyz(x: i32) -> i32 { x }".to_string()).unwrap();
    println!("{:?}", x);
  }

  #[test] fn items_4() {
    let x = string_to_item("type Point = f32;".to_string()).unwrap();
    println!("{:?}", x);
  }

  #[test] fn items_5() {
    let x = string_to_item("type Point = (f64, f64);".to_string()).unwrap();
    println!("{:?}", x);
  }

  #[test] fn items_6() {
    let x = string_to_item("type F = fn(f64) -> f64;".to_string()).unwrap();
    println!("{:?}", x);
  }

  #[test] fn items_7() {
    let x = string_to_item("struct S {name: char, age: i8}".to_string()).unwrap();
    println!("{:?}", x);
  }

  #[test] fn items_8() {
    let x = string_to_item("struct S (char, i8);".to_string()).unwrap();
    println!("{:?}", x);
  }

  #[test] fn items_9() {
    let x = string_to_item("import std::sql::*;".to_string()).unwrap();
    println!("{:?}", x);
  }

  #[test] fn expr_1() {
    let x = string_to_expr("1 + 2".to_string());
  }

  #[test] fn stmt_1() {
    let x = string_to_stmt("let x = 10;".to_string()).unwrap();
    println!("{:?}", x);
  }

  #[test] fn stmt_4() {
    let x = string_to_stmt("var x = 10;".to_string()).unwrap();
    println!("{:?}", x);
  }

  #[test] fn stmt_5() {
    let x = string_to_stmt("let x = if y { 1 } else { 0 };".to_string()).unwrap();
    println!("{:?}", x);
  }

  #[test] fn stmt_6() {
    let x = string_to_stmt("for x in 0..10 { println(\"xx\"); }".to_string()).unwrap();
    println!("{:?}", x);
  }

  #[test]
    fn string_to_tts_1() {
        let tts = string_to_tts("fn a (b : i32) { b; }".to_string());

        let expected = vec![
            TokenTree::Token(sp(0, 2),
                         token::Ident(str_to_ident("fn"),
                         token::IdentStyle::Plain)),
            TokenTree::Token(sp(3, 4),
                         token::Ident(str_to_ident("a"),
                         token::IdentStyle::Plain)),
            TokenTree::Delimited(
                sp(5, 14),
                Rc::new(ast::Delimited {
                    delim: token::DelimToken::Paren,
                    open_span: sp(5, 6),
                    tts: vec![
                        TokenTree::Token(sp(6, 7),
                                     token::Ident(str_to_ident("b"),
                                     token::IdentStyle::Plain)),
                        TokenTree::Token(sp(8, 9),
                                     token::Colon),
                        TokenTree::Token(sp(10, 13),
                                     token::Ident(str_to_ident("i32"),
                                     token::IdentStyle::Plain)),
                    ],
                    close_span: sp(13, 14),
                })),
            TokenTree::Delimited(
                sp(15, 21),
                Rc::new(ast::Delimited {
                    delim: token::DelimToken::Brace,
                    open_span: sp(15, 16),
                    tts: vec![
                        TokenTree::Token(sp(17, 18),
                                     token::Ident(str_to_ident("b"),
                                     token::IdentStyle::Plain)),
                        TokenTree::Token(sp(18, 19),
                                     token::Semi)
                    ],
                    close_span: sp(20, 21),
                }))
        ];

        assert_eq!(tts, expected);
    }
}