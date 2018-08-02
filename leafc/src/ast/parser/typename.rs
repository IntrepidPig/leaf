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

pub fn next_typename(stream: &mut TokenStream) -> ParseResult<TypeName> {
	let typename = if let TokenTree::Token(Token { kind: TokenKind::Name(ref name), .. }) = stream.take_tokentree()? {
		TypeName::from_ident(Identifier::try_from_str(name))
	} else {
		return Ok(None)
	};
	
	Ok(Some(typename))
}