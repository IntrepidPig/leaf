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
		Some(TokenTree::Token(Token::Name(ref name))) => { // TODO parse this properly
			tokens = &tokens[1..];
			TypeName::from_ident(Identifier::from_str(name))
		},
		_ => return Err(ParseError::Other.into()), // Type was not given a name
	};
	
	let mut fields = Vec::new();
	
	match tokens.get(0) {
		Some(TokenTree::Brace(ref type_tokens)) => {
			for field_tokens in separated::parse_separated(type_tokens, |token| token.is_comma())? {
				let name = match field_tokens.get(0) {
					Some(TokenTree::Token(Token::Name(ref name))) => {
						Identifier::from_str(name)
					},
					_ => return Err(ParseError::Other.into()) // Got a field that wasn't a name
				};
				
				match field_tokens.get(1) {
					Some(TokenTree::Token(Token::Symbol(TokenSymbol::Colon))) => {},
					_ => return Err(ParseError::Other.into()) // Field name didn't have a colon after it
				}
								
				let (typename, leftovers) = if let Some(res) = next_type(&field_tokens[2..])? {
					res
				} else {
					return Err(ParseError::Other.into()) // Field didn't specify a type
				};
								
				if !leftovers.is_empty() {
					return Err(ParseError::Other.into()) // Didn't parse field type correctly
				}
				
				fields.push((name, typename.clone()))
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