use ast::parser::*;

pub fn take_module(in_tokens: &[TokenTree]) -> ParseResult<(Identifier, Module)> {
	let mut tokens = in_tokens;

	match tokens.get(0) {
		Some(TokenTree::Token(Token::Keyword(Keyword::Module))) => {
			tokens = &tokens[1..];
		},
		_ => return Ok(None),
	}

	let name = match tokens.get(0) {
		Some(TokenTree::Token(Token::Name(ref name))) => {
			tokens = &tokens[1..];
			Identifier::try_from_str(name)
		},
		_ => return Err(ParseError::Other.into()), // Module was not given a name
	};

	let body = match tokens.get(0) {
		Some(TokenTree::Brace(ref mod_tokens)) => {
			parse(mod_tokens)?
		},
		_ => return Err(ParseError::Other.into()), // needed a a block of code after module
	};

	tokens = &tokens[1..];

	Ok(Some(((
		name,
		Module {
			body,
		}),
		tokens,
	)))
}
