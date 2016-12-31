#![allow(dead_code)]

use std::fmt;
use std::u32;

#[derive(Clone, Copy, Eq, Ord, PartialOrd, PartialEq, Hash, Debug,
         RustcDecodable, RustcEncodable)]
pub struct PackageNum(u32);

/// Item definitions in the currently-compiled crate would have the CrateNum
/// LOCAL_CRATE in their DefId.
pub const LOCAL_PACKAGE: PackageNum = PackageNum(0);

impl PackageNum {
    pub fn new(x: usize) -> PackageNum {
        assert!(x < (u32::MAX as usize));
        PackageNum(x as u32)
    }

    pub fn from_u32(x: u32) -> PackageNum {
        PackageNum(x)
    }

    pub fn as_usize(&self) -> usize {
        self.0 as usize
    }

    pub fn as_u32(&self) -> u32 {
        self.0
    }
}

impl fmt::Display for PackageNum {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

/// A DefIndex is an index into the hir-map for a crate, identifying a
/// particular definition. It should really be considered an interned
/// shorthand for a particular DefPath.
#[derive(Clone, Debug, Eq, Ord, PartialOrd, PartialEq, RustcEncodable,
           RustcDecodable, Hash, Copy)]
pub struct DefIndex(u32);

impl DefIndex {
    pub fn new(x: usize) -> DefIndex {
        assert!(x < (u32::MAX as usize));
        DefIndex(x as u32)
    }

    pub fn from_u32(x: u32) -> DefIndex {
        DefIndex(x)
    }

    pub fn as_usize(&self) -> usize {
        self.0 as usize
    }

    pub fn as_u32(&self) -> u32 {
        self.0
    }
}

/// The package root is always assigned index 0 by the AST Map code,
/// thanks to `NodeCollector::new`.
pub const PACKAGE_DEF_INDEX: DefIndex = DefIndex(0);

/// A DefId identifies a particular *definition*, by combining a package
/// index and a def index.
#[derive(Clone, Eq, Ord, PartialOrd, PartialEq, RustcEncodable, RustcDecodable,
         Hash, Copy)]
pub struct DefId {
    pub package: PackageNum,
    pub index: DefIndex,
}

impl fmt::Debug for DefId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "DefId {{ package: {:?}, node: {:?} }}", self.package, self.index)
    }
}


impl DefId {
    pub fn local(index: DefIndex) -> DefId {
        DefId { package: LOCAL_PACKAGE, index: index }
    }

    pub fn is_local(&self) -> bool {
        self.package == LOCAL_PACKAGE
    }
}
