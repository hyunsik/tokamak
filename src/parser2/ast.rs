use std::fmt;

use token;

/// A name is a part of an identifier, representing a string or gensym. It's
/// the result of interning.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(pub u32);

impl Name {
  pub fn as_str(self) -> token::InternedString {
    token::InternedString::new_from_name(self)
  }
}

impl fmt::Debug for Name {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}({})", self, self.0)
  }
}

impl fmt::Display for Name {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    fmt::Display::fmt(&self.as_str(), f)
  }
}

/// A SyntaxContext represents a chain of macro-expandings
/// and renamings. Each macro expansion corresponds to
/// a fresh u32. This u32 is a reference to a table stored
/// in thread-local storage.
/// The special value EMPTY_CTXT is used to indicate an empty
/// syntax context.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct SyntaxContext(pub u32);

/// An identifier contains a Name (index into the interner
/// table) and a SyntaxContext to track renaming and
/// macro expansion per Flatt et al., "Macros That Work Together"
#[derive(Clone, Copy, Eq)]
pub struct Ident {
  pub name: Name,
  pub ctxt: SyntaxContext
}

pub const EMPTY_CTXT : SyntaxContext = SyntaxContext(0);

impl Ident {
  pub fn new(name: Name, ctxt: SyntaxContext) -> Ident {
    Ident {name: name, ctxt: ctxt}
  }
  pub fn with_empty_ctxt(name: Name) -> Ident {
    Ident {name: name, ctxt: EMPTY_CTXT}
  }
}

impl PartialEq for Ident {
  fn eq(&self, other: &Ident) -> bool {
    if self.ctxt != other.ctxt {
      // There's no one true way to compare Idents. They can be compared
      // non-hygienically `id1.name == id2.name`, hygienically
      // `mtwt::resolve(id1) == mtwt::resolve(id2)`, or even member-wise
      // `(id1.name, id1.ctxt) == (id2.name, id2.ctxt)` depending on the situation.
      // Ideally, PartialEq should not be implemented for Ident at all, but that
      // would be too impractical, because many larger structures (Token, in particular)
      // including Idents as their parts derive PartialEq and use it for non-hygienic
      // comparisons. That's why PartialEq is implemented and defaults to non-hygienic
      // comparison. Hash is implemented too and is consistent with PartialEq, i.e. only
      // the name of Ident is hashed. Still try to avoid comparing idents in your code
      // (especially as keys in hash maps), use one of the three methods listed above
      // explicitly.
      //
      // If you see this panic, then some idents from different contexts were compared
      // non-hygienically. It's likely a bug. Use one of the three comparison methods
      // listed above explicitly.

      panic!("idents with different contexts are compared with operator `==`: \
                {:?}, {:?}.", self, other);
    }

    self.name == other.name
  }
}


impl fmt::Debug for Ident {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}#{}", self.name, self.ctxt.0)
  }
}

impl fmt::Display for Ident {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    fmt::Display::fmt(&self.name, f)
  }
}