use syntax::ast;

pub enum PpMode {
}

#[derive(Clone, Debug)]
pub enum UserIdentifiedItem {
    ItemViaNode(ast::NodeId),
    ItemViaPath(Vec<String>),
}