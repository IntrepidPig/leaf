use ast::parser::*;

pub fn take_functiondef(in_tokens: &[TokenTree]) -> ParseResult<Function> {
	let mut tokens = in_tokens;

	let last_span = match tokens.get(0) {
		Some(TokenTree::Token(Token {
			kind: TokenKind::Keyword(Keyword::Function),
			span,
		})) => {
			tokens = &tokens[1..];
			*span
		},
		_ => return Ok(None),
	};

	let (name, last_span) = match tokens.get(0) {
		Some(TokenTree::Token(Token {
			kind: TokenKind::Name(ref name),
			span,
		})) => {
			tokens = &tokens[1..];
			(Identifier::try_from_str(name), *span)
		},
		t => {
			return Err(ParseError {
				kind: ParseErrorKind::Expected(vec![Expected::Identifier]),
				span: t.map(|t| t.get_span()).unwrap_or(last_span),
			}.into())
		}, // needed a function name
	};

	let (args, last_span) = match tokens.get(0) {
		Some(TokenTree::Block(BlockType::Paren, ref args_tokens, outer_span, inner_span)) => {
			tokens = &tokens[1..];
			if !args_tokens.is_empty() {
				(
					parse_args(args_tokens, *outer_span)?,
					args_tokens[0].get_span(),
				)
			} else {
				(Vec::new(), *inner_span)
			}
		},
		t => {
			return Err(ParseError {
				kind: ParseErrorKind::Expected(vec![Expected::Parentheses]),
				span: t.map(|t| t.get_span()).unwrap_or(last_span),
			}.into())
		}, // needed parens with arguments inside
	};

	let (return_type, last_span) = match tokens.get(0) {
		Some(TokenTree::Token(Token {
			kind: TokenKind::Symbol(TokenSymbol::Colon),
			span,
		})) => {
			tokens = &tokens[1..];
			let (typename, leftovers) = if let Some(res) = next_type(tokens)? {
				res
			} else {
				return Err(ParseError {
					kind: ParseErrorKind::Expected(vec![Expected::Typename]),
					span: *span,
				}.into()); // Expected a type after the colon
			};
			tokens = leftovers;
			(
				Some(typename),
				leftovers.get(0).map(|t| t.get_span()).unwrap_or(*span),
			)
		},
		t => (None, t.map(|t| t.get_span()).unwrap_or(last_span)),
	};

	let block_taker = block::BlockTaker {};
	let (block, leftovers) = if let Some(block) = block_taker.take_expression(tokens, ())? {
		block
	} else {
		return Err(ParseError {
			kind: ParseErrorKind::Expected(vec![Expected::Block]),
			span: last_span,
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

	let last_span = match tokens.get(0) {
		Some(TokenTree::Token(Token {
			kind: TokenKind::Keyword(Keyword::Function),
			span,
		})) => {
			tokens = &tokens[1..];
			*span
		},
		_ => return Ok(None),
	};

	let (name, last_span) = match tokens.get(0) {
		Some(TokenTree::Token(Token {
			kind: TokenKind::Name(ref name),
			span,
		})) => {
			tokens = &tokens[1..];
			(Identifier::try_from_str(name), *span)
		},
		t => {
			return Err(ParseError {
				kind: ParseErrorKind::Expected(vec![Expected::Identifier]),
				span: t.map(|t| t.get_span()).unwrap_or(last_span),
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
				span: t.map(|t| t.get_span()).unwrap_or(last_span),
			}.into())
		}, // needed parens with arguments inside
	};

	let return_type = match tokens.get(0) {
		Some(TokenTree::Token(Token {
			kind: TokenKind::Symbol(TokenSymbol::Colon),
			span,
		})) => {
			tokens = &tokens[1..];
			let (typename, leftovers) = if let Some(res) = next_type(tokens)? {
				res
			} else {
				return Err(ParseError {
					kind: ParseErrorKind::Expected(vec![Expected::Typename]),
					span: *span,
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
	outer_span: Span,
) -> Result<Vec<(Identifier, PathItem<TypeName>)>, Error<ParseError>> {
	let mut args = Vec::new();
	let mut last_span = outer_span;
	for arg_tokens in separated::parse_separated(in_tokens, |token| token.is_comma())? {
		let name = match arg_tokens.get(0) {
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
			}, // Got an argument that wasn't a name
		};

		match arg_tokens.get(1) {
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
			}, // Argument name needed colon after it
		}

		let (typename, leftovers) = if let Some(res) = next_type(&arg_tokens[2..])? {
			// TODO remove panic
			res
		} else {
			return Err(ParseError {
				kind: ParseErrorKind::Expected(vec![Expected::Typename]),
				span: last_span,
			}.into()); // Didn't get type after argument
		};

		if !leftovers.is_empty() {
			return Err(ParseError {
				kind: ParseErrorKind::UnexpectedToken,
				span: leftovers[0].get_span(),
			}.into()); // There were leftover tokens after the parameter type
		}

		args.push((name, typename));
	}

	Ok(args)
}
