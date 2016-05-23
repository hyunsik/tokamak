// Functions dealing with attributes and meta items

use ast::Attribute;

/// A list of attributes, behind a optional box as
/// a space optimization.
pub type ThinAttributes = Option<Box<Vec<Attribute>>>;