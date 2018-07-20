use ast::parser::*;

pub fn take_typedef(in_tokens: &[TokenTree]) -> Result<Option<(Type, &[TokenTree])>, Error<ParseError>> {
	let mut tokens = in_tokens;
	
	match tokens.get(0) {
		Some(TokenTree::Token(Token::Keyword(Keyword::Type))) => {
			tokens = &tokens[1..];
		},
		_ => return Ok(None),
	}
	
	let name = match tokens.get(0) {
		Some(TokenTree::Token(Token::Name(ref name))) => {
			tokens = &tokens[1..];
			name.clone()
		},
		_ => return Err(ParseError::Other.into()), // Type was not given a name
	};
	
	let mut fields = Vec::new();
	
	match tokens.get(0) {
		Some(TokenTree::Brace(ref tokens)) => {
			for field_tokens in separated::parse_separated(tokens, |token| token.is_comma())? {
				let name = match field_tokens.get(0) {
					Some(TokenTree::Token(Token::Name(ref name))) => {
						name.clone()
					},
					_ => return Err(ParseError::Other.into()) // Got a field that wasn't a name
				};
				match field_tokens.get(1) {
					Some(TokenTree::Token(Token::Symbol(TokenSymbol::Colon))) => {},
					_ => return Err(ParseError::Other.into()) // Field name didn't have a colon after it
				}
				let typename = match field_tokens.get(2) {
					Some(TokenTree::Token(Token::Name(ref name))) => {
						name.clone()
					},
					_ => return Err(ParseError::Other.into()) // Field didn't specify a type
				};
				fields.push((name.clone(), typename.clone()))
			}
		},
		_ => return Err(ParseError::Other.into()) // needed a a block with fields
	};
	
	tokens = &tokens[1..];
	
	Ok(Some((Type {
		name,
		members: fields,
	}, tokens)))
}