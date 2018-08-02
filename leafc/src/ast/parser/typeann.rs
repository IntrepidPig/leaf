use ast::parser::*;

pub fn next_typeann(stream: &mut TokenStream) -> ParseResult<PathItem<TypeName>> {
	if let Some(TokenTree::Token(Token {
		kind: TokenKind::Symbol(Symbol::Colon),
		..
	})) = stream.opt_next_tokentree()?
	{
	} else {
		return Ok(None);
	}

	let typename = if let Some(typename) = next_pathitem(stream, next_typename)? {
		typename
	} else {
		return Err(ParseError::expected(vec![Expected::Typename], stream).into());
	};

	Ok(Some(typename))
}
