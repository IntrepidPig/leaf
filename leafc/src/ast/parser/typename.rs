use ast::parser::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeName {
	pub name: Identifier,
}

impl TypeName {
	pub fn from_ident(identifier: Identifier) -> Self {
		TypeName { name: identifier }
	}
}