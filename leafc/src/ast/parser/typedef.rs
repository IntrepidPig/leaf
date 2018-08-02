use ast::parser::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDefinition {
	pub name: TypeName,
	pub members: Vec<(Identifier, PathItem<TypeName>)>,
}

pub fn next_typedef(stream: &mut TokenStream) -> ParseResult<TypeDefinition> {
	if let TokenTree::Token(Token { kind: TokenKind::Keyword(Keyword::Type), .. }) = stream.take_tokentree()? { } else {
		return Ok(None)
	}
	
	let name = if let TokenTree::Token(Token { kind: TokenKind::Name(ref name), .. }) = stream.take_tokentree()? {
		TypeName::from_ident(Identifier::try_from_str(name))
	} else {
		return Err(ParseError::expected(vec![Expected::Identifier], &*stream).into())
	};
	
	let mut members: Vec<(Identifier, PathItem<TypeName>)> = Vec::new();
	if let TokenTree::Block(Bracket::Curly, ref mut block_token_stream, _, _) = stream.take_tokentree()? {
		let fields_token_streams = separated::parse_separated(block_token_stream, |t| t.is_comma())?;
		for mut field_token_stream in fields_token_streams {
			let ident = if let TokenTree::Token(Token { kind: TokenKind::Name(ref name), .. }) = field_token_stream.take_tokentree()? {
				Identifier::try_from_str(name)
			} else {
				return Err(ParseError::expected(vec![Expected::Identifier], &field_token_stream).into());
			};
			
			let typename = if let Some(typename) = typeann::next_typeann(&mut field_token_stream)? {
				typename
			} else {
				return Err(ParseError::expected(vec![Expected::Colon], &field_token_stream).into());
			};
			
			members.push((ident, typename));
		}
	}
	
	Ok(Some(TypeDefinition {
		name,
		members,
	}))
}
