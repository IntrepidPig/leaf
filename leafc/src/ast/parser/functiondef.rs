use ast::parser::*;

pub fn take_functiondef(in_tokens: &[TokenTree]) -> ParseResult<Function> {
	let mut tokens = in_tokens;

	let last_location = match tokens.get(0) {
		Some(TokenTree::Token(Token {
			kind: TokenKind::Keyword(Keyword::Function),
			location,
		})) => {
			tokens = &tokens[1..];
			*location
		},
		_ => return Ok(None),
	};

	let (name, last_location) = match tokens.get(0) {
		Some(TokenTree::Token(Token {
			kind: TokenKind::Name(ref name),
			location,
		})) => {
			tokens = &tokens[1..];
			(Identifier::try_from_str(name), *location)
		},
		t => {
			return Err(ParseError {
				kind: ParseErrorKind::Expected(vec![Expected::Identifier]),
				location: t.map(|t| t.get_location()).unwrap_or(last_location),
			}.into())
		}, // needed a function name
	};

	let (args, last_location) = match tokens.get(0) {
		Some(TokenTree::Block(BlockType::Paren, ref args_tokens, start_location, end_location)) => {
			tokens = &tokens[1..];
			if !args_tokens.is_empty() {
				(
					parse_args(args_tokens, *start_location)?,
					args_tokens[0].get_location(),
				)
			} else {
				(Vec::new(), *end_location)
			}
		},
		t => {
			return Err(ParseError {
				kind: ParseErrorKind::Expected(vec![Expected::Parentheses]),
				location: t.map(|t| t.get_location()).unwrap_or(last_location),
			}.into())
		}, // needed parens with arguments inside
	};

	let (return_type, last_location) = match tokens.get(0) {
		Some(TokenTree::Token(Token {
			kind: TokenKind::Symbol(TokenSymbol::Colon),
			location,
		})) => {
			tokens = &tokens[1..];
			let (typename, leftovers) = if let Some(res) = next_type(tokens)? {
				res
			} else {
				return Err(ParseError {
					kind: ParseErrorKind::Expected(vec![Expected::Typename]),
					location: *location,
				}.into()); // Expected a type after the colon
			};
			tokens = leftovers;
			(
				Some(typename),
				leftovers
					.get(0)
					.map(|t| t.get_location())
					.unwrap_or(*location),
			)
		},
		t => (None, t.map(|t| t.get_location()).unwrap_or(last_location)),
	};

	let block_taker = block::BlockTaker {};
	let (block, leftovers) = if let Some(block) = block_taker.take_expression(tokens, ())? {
		block
	} else {
		return Err(ParseError {
			kind: ParseErrorKind::Expected(vec![Expected::Block]),
			location: last_location,
		}.into()); // needed a block after the args or return type
	};

	let block = match block {
		Expression::Block(block) => *block,
		_ => unreachable!(), // Got an expression that wasn't a block
	};

	tokens = leftovers;

	Ok(Some((
		Function {
			name,
			args,
			return_type,
			body: block,
		},
		tokens,
	)))
}

pub fn take_externfn(in_tokens: &[TokenTree]) -> ParseResult<ExternFunction> {
	let mut tokens = in_tokens;

	match tokens.get(0) {
		Some(TokenTree::Token(Token {
			kind: TokenKind::Keyword(Keyword::Extern),
			..
		})) => {
			tokens = &tokens[1..];
		},
		_ => return Ok(None),
	}

	let last_location = match tokens.get(0) {
		Some(TokenTree::Token(Token {
			kind: TokenKind::Keyword(Keyword::Function),
			location,
		})) => {
			tokens = &tokens[1..];
			*location
		},
		_ => return Ok(None),
	};

	let (name, last_location) = match tokens.get(0) {
		Some(TokenTree::Token(Token {
			kind: TokenKind::Name(ref name),
			location,
		})) => {
			tokens = &tokens[1..];
			(Identifier::try_from_str(name), *location)
		},
		t => {
			return Err(ParseError {
				kind: ParseErrorKind::Expected(vec![Expected::Identifier]),
				location: t.map(|t| t.get_location()).unwrap_or(last_location),
			}.into())
		}, // needed a function name
	};

	let (args, _last_location) = match tokens.get(0) {
		Some(TokenTree::Block(BlockType::Paren, ref args_tokens, start_location, end_location)) => {
			tokens = &tokens[1..];
			(
				if !args_tokens.is_empty() {
					parse_args(args_tokens, *start_location)?
				} else {
					Vec::new()
				},
				*end_location,
			)
		},
		t => {
			return Err(ParseError {
				kind: ParseErrorKind::Expected(vec![Expected::Parentheses]),
				location: t.map(|t| t.get_location()).unwrap_or(last_location),
			}.into())
		}, // needed parens with arguments inside
	};

	let return_type = match tokens.get(0) {
		Some(TokenTree::Token(Token {
			kind: TokenKind::Symbol(TokenSymbol::Colon),
			location,
		})) => {
			tokens = &tokens[1..];
			let (typename, leftovers) = if let Some(res) = next_type(tokens)? {
				res
			} else {
				return Err(ParseError {
					kind: ParseErrorKind::Expected(vec![Expected::Typename]),
					location: *location,
				}.into()); // Expected a type after the colon
			};
			tokens = leftovers;
			Some(typename)
		},
		_ => None,
	};

	Ok(Some((
		ExternFunction {
			name,
			args,
			return_type,
		},
		tokens,
	)))
}

fn parse_args(
	in_tokens: &[TokenTree],
	start_location: Location,
) -> Result<Vec<(Identifier, PathItem<TypeName>)>, Error<ParseError>> {
	let mut args = Vec::new();
	let mut last_location = start_location;
	for arg_tokens in separated::parse_separated(in_tokens, |token| token.is_comma())? {
		let name = match arg_tokens.get(0) {
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
			}, // Got an argument that wasn't a name
		};

		match arg_tokens.get(1) {
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
			}, // Argument name needed colon after it
		}

		let (typename, leftovers) = if let Some(res) = next_type(&arg_tokens[2..])? {
			// TODO remove panic
			res
		} else {
			return Err(ParseError {
				kind: ParseErrorKind::Expected(vec![Expected::Typename]),
				location: last_location,
			}.into()); // Didn't get type after argument
		};

		if !leftovers.is_empty() {
			return Err(ParseError {
				kind: ParseErrorKind::UnexpectedToken,
				location: leftovers[0].get_location(),
			}.into()); // There were leftover tokens after the parameter type
		}

		args.push((name, typename));
	}

	Ok(args)
}
