use ast::parser::*;

pub fn take_use(
	in_tokens: &[TokenTree],
) -> Result<Option<(PathItem<Identifier>, &[TokenTree])>, Error<ParseError>> {
	let mut tokens = in_tokens;

	match tokens.get(0) {
		Some(TokenTree::Token(Token::Keyword(Keyword::Use))) => {
			tokens = &tokens[1..];
		},
		_ => return Ok(None),
	}

	let (use_path, leftovers) = if let Some(res) = pathitem::next_path(tokens)? {
		res
	} else {
		return Err(ParseError::Other.into());
	};

	tokens = leftovers;

	let item = match tokens.get(0) {
		Some(TokenTree::Token(Token::Name(ref name))) => {
			tokens = &tokens[1..];
			Identifier::from_str(name)
		},
		_ => return Err(ParseError::Other.into()), // Expected an identifier after the use path
	};

	Ok(Some((
		PathItem {
			module_path: use_path,
			item,
		},
		tokens,
	)))
}
