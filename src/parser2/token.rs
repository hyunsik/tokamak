pub use self::BinOpToken::*;
pub use self::DelimToken::*;
pub use self::IdentStyle::*;
pub use self::Token::*;

use std::fmt;
use std::ops::Deref;
use std::rc::Rc;

use ast;
use interner::{self, StrInterner, RcStr};

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum BinOpToken {
  Plus,
  Minus,
  Star,
  Slash,
  LShift,
  RShift
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum DelimToken {
  /// A round parenthesis: `(` or `)`
  Paren,
  /// A square bracket: `[` or `]`
  Bracket,
  /// A curly brace: `{` or `}`
  Brace,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub enum IdentStyle {
  /// `::` follows the identifier with no whitespace in-between.
  ModName,
  Plain,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Token {
  /* Expression-operator symbols. */
  Eq,
  EqEq,
  Ne,
  Lt,
  Le,
  Ge,
  Gt,
  AndAnd,
  OrOr,
  Not,
  Tilde,
  BinOp(BinOpToken),
  BinOpEq(BinOpToken), // e.g. '+='

  /* Structural symbols */
  At,       // @
  Colon,    // :
  SemiColon,// ;
  Comma,    // ,
  Dot,      // .
  DotDot,   // ..
  DotDotDot,// ...
  Dollar,   // $
  Pound,    // #
  Question, // ?
  ModSep,   // ::
  LArrow,   // <-
  RArrow,   // ->
  FatArrow, // =>

  /// An opening delimiter, eg. `{`
  OpenDelim(DelimToken),
  /// A closing delimiter, eg. `}`
  CloseDelim(DelimToken),

  /* Name components */
  Ident(ast::Ident, IdentStyle),
  Underscore,

  /// Whitespace
  Whitespace,
  /// Comment
  Comment,

  /// End of file
  Eof,
}

impl Token {
  /// Returns `true` if the token is an identifier.
  pub fn is_ident(&self) -> bool {
    match *self {
      Ident(_, _) => true,
      _           => false,
    }
  }

  /// Returns `true` if the token is a given keyword, `kw`.
  #[allow(non_upper_case_globals)]
  pub fn is_keyword(&self, kw: keywords::Keyword) -> bool {
    match *self {
      Ident(sid, Plain) => kw.to_name() == sid.name,
      _                      => false,
    }
  }
}

// Get the first "argument"
macro_rules! first {
    ( $first:expr, $( $remainder:expr, )* ) => ( $first )
}

// Get the last "argument" (has to be done recursively to avoid phoney local ambiguity error)
macro_rules! last {
    ( $first:expr, $( $remainder:expr, )+ ) => ( last!( $( $remainder, )+ ) );
    ( $first:expr, ) => ( $first )
}

// In this macro, there is the requirement that the name (the number) must be monotonically
// increasing by one in the special identifiers, starting at 0; the same holds for the keywords,
// except starting from the next number instead of zero, and with the additional exception that
// special identifiers are *also* allowed (they are deduplicated in the important place, the
// interner), an exception which is demonstrated by "static" and "self".
macro_rules! declare_special_idents_and_keywords {(
    // So now, in these rules, why is each definition parenthesised?
    // Answer: otherwise we get a spurious local ambiguity bug on the "}"
    pub mod special_idents {
        $( ($si_name:expr, $si_static:ident, $si_str:expr); )*
    }

    pub mod keywords {
        'strict:
        $( ($sk_name:expr, $sk_variant:ident, $sk_str:expr); )*
        'reserved:
        $( ($rk_name:expr, $rk_variant:ident, $rk_str:expr); )*
    }
) => {
    const STRICT_KEYWORD_START: ast::Name = first!($( ast::Name($sk_name), )*);
    const STRICT_KEYWORD_FINAL: ast::Name = last!($( ast::Name($sk_name), )*);
    const RESERVED_KEYWORD_START: ast::Name = first!($( ast::Name($rk_name), )*);
    const RESERVED_KEYWORD_FINAL: ast::Name = last!($( ast::Name($rk_name), )*);

    pub mod special_idents {
        use ast;
        $(
            #[allow(non_upper_case_globals)]
            pub const $si_static: ast::Ident = ast::Ident {
                name: ast::Name($si_name),
                ctxt: ast::EMPTY_CTXT,
            };
         )*
    }

    pub mod special_names {
        use ast;
        $(
            #[allow(non_upper_case_globals)]
            pub const $si_static: ast::Name = ast::Name($si_name);
        )*
    }

    /// All the valid words that have meaning in the Rust language.
    ///
    /// Rust keywords are either 'strict' or 'reserved'.  Strict keywords may not
    /// appear as identifiers at all. Reserved keywords are not used anywhere in
    /// the language and may not appear as identifiers.
    pub mod keywords {
        pub use self::Keyword::*;
        use ast;

        #[derive(Copy, Clone, PartialEq, Eq)]
        pub enum Keyword {
            $( $sk_variant, )*
            $( $rk_variant, )*
        }

        impl Keyword {
            pub fn to_name(&self) -> ast::Name {
                match *self {
                    $( $sk_variant => ast::Name($sk_name), )*
                    $( $rk_variant => ast::Name($rk_name), )*
                }
            }
        }
    }

    fn mk_fresh_ident_interner() -> IdentInterner {
        // The indices here must correspond to the numbers in
        // special_idents, in Keyword to_name(), and in static
        // constants below.
        let mut init_vec = Vec::new();
        $(init_vec.push($si_str);)*
        $(init_vec.push($sk_str);)*
        $(init_vec.push($rk_str);)*
        interner::StrInterner::prefill(&init_vec[..])
    }
}}

pub const SELF_KEYWORD_NAME_NUM: u32 = 1;
const STATIC_KEYWORD_NAME_NUM: u32 = 2;
const SUPER_KEYWORD_NAME_NUM: u32 = 3;
const SELF_TYPE_KEYWORD_NAME_NUM: u32 = 10;

// NB: leaving holes in the ident table is bad! a different ident will get
// interned with the id from the hole, but it will be between the min and max
// of the reserved words, and thus tagged as "reserved".

declare_special_idents_and_keywords! {
    pub mod special_idents {
        // These ones are statics
        (0,                          invalid,                "");
        (super::SELF_KEYWORD_NAME_NUM,   self_,              "self");
        (super::STATIC_KEYWORD_NAME_NUM, statik,             "static");
        (super::SUPER_KEYWORD_NAME_NUM, super_,              "super");
        (4,                          static_lifetime,        "'static");

        // for matcher NTs
        (5,                          tt,                     "tt");
        (6,                          matchers,               "matchers");

        // outside of libsyntax
        (7,                          clownshoe_abi,          "__rust_abi");
        (8,                          opaque,                 "<opaque>");
        (9,                          unnamed_field,          "<unnamed_field>");
        (super::SELF_TYPE_KEYWORD_NAME_NUM, type_self,       "Self");
        (11,                         prelude_import,         "prelude_import");
        (12,                         DEFAULT,                "default");
    }

    pub mod keywords {
        // These ones are variants of the Keyword enum

        'strict:
        (13,                         As,         "as");
        (14,                         Break,      "break");
        (15,                         Crate,      "crate");
        (16,                         Else,       "else");
        (17,                         Enum,       "enum");
        (18,                         Extern,     "extern");
        (19,                         False,      "false");
        (20,                         Fn,         "fn");
        (21,                         For,        "for");
        (22,                         If,         "if");
        (23,                         Impl,       "impl");
        (24,                         In,         "in");
        (25,                         Let,        "let");
        (26,                         Loop,       "loop");
        (27,                         Match,      "match");
        (28,                         Mod,        "mod");
        (29,                         Move,       "move");
        (30,                         Mut,        "mut");
        (31,                         Pub,        "pub");
        (32,                         Ref,        "ref");
        (33,                         Return,     "return");
        // Static and Self are also special idents (prefill de-dupes)
        (super::STATIC_KEYWORD_NAME_NUM, Static, "static");
        (super::SELF_KEYWORD_NAME_NUM, SelfValue, "self");
        (super::SELF_TYPE_KEYWORD_NAME_NUM, SelfType, "Self");
        (34,                         Struct,     "struct");
        (super::SUPER_KEYWORD_NAME_NUM, Super,   "super");
        (35,                         True,       "true");
        (36,                         Trait,      "trait");
        (37,                         Type,       "type");
        (38,                         Unsafe,     "unsafe");
        (39,                         Import,     "import");
        (40,                         While,      "while");
        (41,                         Continue,   "continue");
        (42,                         Box,        "box");
        (43,                         Const,      "const");
        (44,                         Where,      "where");
        (45,                         Var,        "var");
        'reserved:
        (46,                         Virtual,    "virtual");
        (47,                         Proc,       "proc");
        (48,                         Alignof,    "alignof");
        (49,                         Become,     "become");
        (50,                         Offsetof,   "offsetof");
        (51,                         Priv,       "priv");
        (52,                         Pure,       "pure");
        (53,                         Sizeof,     "sizeof");
        (54,                         Typeof,     "typeof");
        (55,                         Unsized,    "unsized");
        (56,                         Yield,      "yield");
        (57,                         Do,         "do");
        (58,                         Abstract,   "abstract");
        (59,                         Final,      "final");
        (60,                         Override,   "override");
        (61,                         Macro,      "macro");
    }
}

// looks like we can get rid of this completely...
pub type IdentInterner = StrInterner;

// if an interner exists in TLS, return it. Otherwise, prepare a
// fresh one.
// FIXME(eddyb) #8726 This should probably use a thread-local reference.
pub fn get_ident_interner() -> Rc<IdentInterner> {
  thread_local!(static KEY: Rc<::token::IdentInterner> = {
        Rc::new(mk_fresh_ident_interner())
    });
  KEY.with(|k| k.clone())
}

/// Reset the ident interner to its initial state.
pub fn reset_ident_interner() {
  let interner = get_ident_interner();
  interner.reset(mk_fresh_ident_interner());
}

/// Represents a string stored in the thread-local interner. Because the
/// interner lives for the life of the thread, this can be safely treated as an
/// immortal string, as long as it never crosses between threads.
///
/// FIXME(pcwalton): You must be careful about what you do in the destructors
/// of objects stored in TLS, because they may run after the interner is
/// destroyed. In particular, they must not access string contents. This can
/// be fixed in the future by just leaking all strings until thread death
/// somehow.
#[derive(Clone, PartialEq, Hash, PartialOrd, Eq, Ord)]
pub struct InternedString {
  string: RcStr,
}

impl InternedString {
  #[inline]
  pub fn new(string: &'static str) -> InternedString {
    InternedString {
      string: RcStr::new(string),
    }
  }

  #[inline]
  fn new_from_rc_str(string: RcStr) -> InternedString {
    InternedString {
      string: string,
    }
  }

  #[inline]
  pub fn new_from_name(name: ast::Name) -> InternedString {
    let interner = get_ident_interner();
    InternedString::new_from_rc_str(interner.get(name))
  }
}

impl Deref for InternedString {
  type Target = str;

  fn deref(&self) -> &str { &*self.string }
}

impl fmt::Debug for InternedString {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    fmt::Debug::fmt(&self.string, f)
  }
}

impl fmt::Display for InternedString {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    fmt::Display::fmt(&self.string, f)
  }
}

/// Interns and returns the string contents of an identifier, using the
/// thread-local interner.
#[inline]
pub fn intern_and_get_ident(s: &str) -> InternedString {
  intern(s).as_str()
}

/// Maps a string to its interned representation.
#[inline]
pub fn intern(s: &str) -> ast::Name {
  get_ident_interner().intern(s)
}

/// gensym's a new usize, using the current interner.
#[inline]
pub fn gensym(s: &str) -> ast::Name {
  get_ident_interner().gensym(s)
}

/// Maps a string to an identifier with an empty syntax context.
#[inline]
pub fn str_to_ident(s: &str) -> ast::Ident {
  ast::Ident::with_empty_ctxt(intern(s))
}

/// Maps a string to a gensym'ed identifier.
#[inline]
pub fn gensym_ident(s: &str) -> ast::Ident {
  ast::Ident::with_empty_ctxt(gensym(s))
}