use ast::parser::*;

pub fn take_typedef(in_tokens: &[TokenTree]) -> ParseResult<Type> {
	let mut tokens = in_tokens;

	let last_span = match tokens.get(0) {
		Some(TokenTree::Token(Token {
			kind: TokenKind::Keyword(Keyword::Type),
			span,
		})) => {
			tokens = &tokens[1..];
			*span
		},
		_ => return Ok(None),
	};

	let (name, mut last_span) = match tokens.get(0) {
		Some(TokenTree::Token(Token {
			kind: TokenKind::Name(ref name),
			span,
		})) => {
			// TODO parse this properly
			tokens = &tokens[1..];
			(TypeName::from_ident(Identifier::try_from_str(name)), *span)
		},
		t => {
			return Err(ParseError {
				kind: ParseErrorKind::Expected(vec![Expected::Identifier]),
				span: t.map(|t| t.get_span()).unwrap_or(last_span),
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
						span,
					})) => {
						last_span = *span;
						Identifier::try_from_str(name)
					},
					t => {
						return Err(ParseError {
							kind: ParseErrorKind::Expected(vec![Expected::Identifier]),
							span: t.map(|t| t.get_span()).unwrap_or(last_span),
						}.into())
					}, // Got a field that wasn't a name
				};

				match field_tokens.get(1) {
					Some(TokenTree::Token(Token {
						kind: TokenKind::Symbol(TokenSymbol::Colon),
						span,
					})) => {
						last_span = *span;
					},
					t => {
						return Err(ParseError {
							kind: ParseErrorKind::Expected(vec![Expected::Colon]),
							span: t.map(|t| t.get_span()).unwrap_or(last_span),
						}.into())
					}, // Field name didn't have a colon after it
				}

				let (typename, leftovers) = if let Some(res) = next_type(&field_tokens[2..])? {
					res
				} else {
					return Err(ParseError {
						kind: ParseErrorKind::Expected(vec![Expected::Typename]),
						span: last_span,
					}.into()); // Field didn't specify a type
				};

				if !leftovers.is_empty() {
					return Err(ParseError {
						kind: ParseErrorKind::UnexpectedToken,
						span: leftovers[0].get_span(),
					}.into()); // Didn't parse field type correctly
				}

				fields.push((name, typename.clone()))
			}
		},
		t => {
			return Err(ParseError {
				kind: ParseErrorKind::Expected(vec![Expected::Block]),
				span: t.map(|t| t.get_span()).unwrap_or(last_span),
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
