use ast::parser::*;

pub fn take_module(in_tokens: &[TokenTree]) -> ParseResult<(Identifier, Module)> {
	let mut tokens = in_tokens;

	let last_location = match tokens.get(0) {
		Some(TokenTree::Token(Token { kind: TokenKind::Keyword(Keyword::Module), location })) => {
			tokens = &tokens[1..];
			*location
		},
		_ => return Ok(None),
	};

	let (name, last_location) = match tokens.get(0) {
		Some(TokenTree::Token(Token { kind: TokenKind::Name(ref name), location })) => {
			tokens = &tokens[1..];
			(Identifier::try_from_str(name), *location)
		},
		t => return Err(ParseError {
			kind: ParseErrorKind::Expected(vec![Expected::Identifier]),
			location: t.map(|t| t.get_location()).unwrap_or(last_location),
		}.into()), // Module was not given a name
	};

	let body = match tokens.get(0) {
		Some(TokenTree::Block(BlockType::Brace, ref mod_tokens, start_location, end_location)) => parse(mod_tokens)?,
		t => return Err(ParseError {
			kind: ParseErrorKind::Expected(vec![Expected::Block]),
			location: t.map(|t| t.get_location()).unwrap_or(last_location),
		}.into()), // needed a a block of code after module
	};

	tokens = &tokens[1..];

	Ok(Some(((name, Module { body }), tokens)))
}
