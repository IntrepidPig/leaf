use ast::parser::*;

pub fn take_typedef(in_tokens: &[TokenTree]) -> ParseResult<Type> {
	let mut tokens = in_tokens;

	let last_location = match tokens.get(0) {
		Some(TokenTree::Token(Token {
			kind: TokenKind::Keyword(Keyword::Type),
			location,
		})) => {
			tokens = &tokens[1..];
			*location
		},
		_ => return Ok(None),
	};

	let (name, mut last_location) = match tokens.get(0) {
		Some(TokenTree::Token(Token {
			kind: TokenKind::Name(ref name),
			location,
		})) => {
			// TODO parse this properly
			tokens = &tokens[1..];
			(
				TypeName::from_ident(Identifier::try_from_str(name)),
				*location,
			)
		},
		t => {
			return Err(ParseError {
				kind: ParseErrorKind::Expected(vec![Expected::Identifier]),
				location: t.map(|t| t.get_location()).unwrap_or(last_location),
			}.into())
		}, // Type was not given a name
	};

	let mut fields = Vec::new();

	match tokens.get(0) {
		Some(TokenTree::Block(BlockType::Brace, ref type_tokens, _start_location, _end_location)) => {
			for field_tokens in separated::parse_separated(type_tokens, |token| token.is_comma())? {
				let name = match field_tokens.get(0) {
					Some(TokenTree::Token(Token {
						kind: TokenKind::Name(ref name),
						location,
					})) => {
						last_location = *location;
						Identifier::try_from_str(name)
					},
					t => {
						return Err(ParseError {
							kind: ParseErrorKind::Expected(vec![Expected::Identifier]),
							location: t.map(|t| t.get_location()).unwrap_or(last_location),
						}.into())
					}, // Got a field that wasn't a name
				};

				match field_tokens.get(1) {
					Some(TokenTree::Token(Token {
						kind: TokenKind::Symbol(TokenSymbol::Colon),
						location,
					})) => {
						last_location = *location;
					},
					t => {
						return Err(ParseError {
							kind: ParseErrorKind::Expected(vec![Expected::Colon]),
							location: t.map(|t| t.get_location()).unwrap_or(last_location),
						}.into())
					}, // Field name didn't have a colon after it
				}

				let (typename, leftovers) = if let Some(res) = next_type(&field_tokens[2..])? {
					res
				} else {
					return Err(ParseError {
						kind: ParseErrorKind::Expected(vec![Expected::Typename]),
						location: last_location,
					}.into()); // Field didn't specify a type
				};

				if !leftovers.is_empty() {
					return Err(ParseError {
						kind: ParseErrorKind::UnexpectedToken,
						location: leftovers[0].get_location(),
					}.into()); // Didn't parse field type correctly
				}

				fields.push((name, typename.clone()))
			}
		},
		t => {
			return Err(ParseError {
				kind: ParseErrorKind::Expected(vec![Expected::Block]),
				location: t.map(|t| t.get_location()).unwrap_or(last_location),
			}.into())
		}, // needed a a block with fields
	};

	tokens = &tokens[1..];

	Ok(Some((
		Type {
			name,
			members: fields,
		},
		tokens,
	)))
}
