# Coding Standard and Conventions for Rust

## Naming

### File Names

### Type Names

### Variable Names

### Enum Names

### Struct and Trait

### Function

### Constant and Static Variables
 * Capital letter or underbar ``_``; e.g., ``ROWBATCH_SIZE``.

### Module name
 * A module name must use low case letter or underbar ``_``.

## Deriving

Trait names in ``derive`` must be sorted in an lexicographic order.
```
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct FuncSignature
{
  ...
}
```

## Indents and Spaces

## Wrapping and Braces

## Imports
 * In each module, Imports declarations must be the top of the source code.
 * Imports should be declared into the following two parts in the following orders.
   * Rust standard or Third party crates
   * Tokamak crates (or modules)
 * Import declarations in each part must be sorted in an lexicographic order of crate names.

Example:
```rust
// Third party crates, sorted in a lexicographic order of names.
extern crate alloc;
extern crate itertools;
extern crate libc;
extern crate uuid;

// Tokamak crates, and they are sorted in a lexicographic order of names.
extern crate algebra;
extern crate common;
extern crate util;
```

## Documentations
