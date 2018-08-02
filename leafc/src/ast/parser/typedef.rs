use ast::parser::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDefinition {
	pub name: TypeName,
	pub members: Vec<(Identifier, PathItem<TypeName>)>,
}

pub fn next_typedef(stream: &mut TokenStream) -> ParseResult<TypeDefinition> {
	unimplemented!()
}
