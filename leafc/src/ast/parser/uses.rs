use ast::parser::*;

pub fn take_use<'a>(in_tokens: &'a [TokenTree]) -> ParseResult<'a, PathItem<Identifier>> {
	let mut tokens = in_tokens;

	let last_location = match tokens.get(0) {
		Some(TokenTree::Token(Token {
			kind: TokenKind::Keyword(Keyword::Use),
			location,
		})) => {
			tokens = &tokens[1..];
			*location
		},
		_ => return Ok(None),
	};

	let (use_path, leftovers) = if let Some(res) = pathitem::next_path(tokens)? {
		res
	} else {
		return Err(ParseError {
			kind: ParseErrorKind::Expected(vec![Expected::ModulePath]),
			location: tokens
				.get(0)
				.map(|t| t.get_location())
				.unwrap_or(last_location),
		}.into());
	};

	tokens = leftovers;

	let item = match tokens.get(0) {
		Some(TokenTree::Token(Token {
			kind: TokenKind::Name(ref name),
			..
		})) => {
			tokens = &tokens[1..];
			Identifier::try_from_str(name)
		},
		// Expected an identifier after the use path
		t => {
			return Err(ParseError {
				kind: ParseErrorKind::Expected(vec![Expected::Identifier]),
				location: t.map(|t| t.get_location()).unwrap_or(last_location),
			}.into())
		},
	};

	Ok(Some((
		PathItem {
			module_path: use_path,
			item,
		},
		tokens,
	)))
}
