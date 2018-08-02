use ast::parser::*;

pub fn take_use(stream: &mut TokenStream) -> ParseResult<PathItem<Identifier>> {
	if let TokenTree::Token(Token {
		kind: TokenKind::Keyword(Keyword::Use),
		..
	}) = stream.take_tokentree()?
	{
	} else {
		return Ok(None);
	}

	let u = if let Some(u) = next_pathitem(stream, |stream: &mut TokenStream| {
		if let TokenTree::Token(Token {
			kind: TokenKind::Name(ref name),
			..
		}) = stream.take_tokentree()?
		{
			Ok(Some(Identifier::try_from_str(name)))
		} else {
			Ok(None)
		}
	})? {
		u
	} else {
		return Err(ParseError::expected(vec![Expected::ModulePath], stream).into());
	};

	Ok(Some(u))
}
