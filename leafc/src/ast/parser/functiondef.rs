use ast::parser::*;

pub fn take_functiondef(
	in_tokens: &[TokenTree],
) -> Result<Option<(Function, &[TokenTree])>, Error<ParseError>> {
	let mut tokens = in_tokens;

	match tokens.get(0) {
		Some(TokenTree::Token(Token::Keyword(Keyword::Function))) => {
			tokens = &tokens[1..];
		},
		_ => return Ok(None),
	}

	let name = match tokens.get(0) {
		Some(TokenTree::Token(Token::Name(ref name))) => {
			tokens = &tokens[1..];
			Identifier::from_str(name)
		},
		_ => return Err(ParseError::Other.into()), // needed a function name
	};

	let args = match tokens.get(0) {
		Some(TokenTree::Paren(ref args_tokens)) => {
			tokens = &tokens[1..];
			if !args_tokens.is_empty() {
				parse_args(args_tokens)?
			} else {
				Vec::new()
			}
		},
		_ => return Err(ParseError::Other.into()), // needed parens with arguments inside
	};

	let return_type = match tokens.get(0) {
		Some(TokenTree::Token(Token::Symbol(TokenSymbol::Colon))) => {
			tokens = &tokens[1..];
			let (typename, leftovers) = if let Some(res) = next_type(tokens)? {
				res
			} else {
				return Err(ParseError::Other.into()); // Expected a type after the colon
			};
			tokens = leftovers;
			Some(typename)
		},
		_ => None,
	};

	let block_taker = block::BlockTaker {};
	let (block, leftovers) = if let Some(block) = block_taker.take_expression(tokens, ())? {
		block
	} else {
		return Err(ParseError::Other.into()); // needed a block after the args or return type
	};

	let block = match block {
		Expression::Block(block) => *block,
		_ => return Err(ParseError::Other.into()), // Got an expression that wasn't a block
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

fn parse_args(
	in_tokens: &[TokenTree],
) -> Result<Vec<(Identifier, PathItem<TypeName>)>, Error<ParseError>> {
	let mut args = Vec::new();
	for arg_tokens in separated::parse_separated(in_tokens, |token| token.is_comma())? {
		let name = match arg_tokens.get(0) {
			Some(TokenTree::Token(Token::Name(ref name))) => Identifier::from_str(name),
			_ => return Err(ParseError::Other.into()), // Got an argument that wasn't a name
		};

		match arg_tokens.get(1) {
			Some(TokenTree::Token(Token::Symbol(TokenSymbol::Colon))) => {},
			_ => return Err(ParseError::Other.into()), // Argument name needed colon after it
		}

		let (typename, leftovers) = if let Some(res) = next_type(&arg_tokens[2..])? {
			res
		} else {
			return Err(ParseError::Other.into()); // Didn't get type after argument
		};

		if !leftovers.is_empty() {
			return Err(ParseError::Other.into()); // There were leftover tokens after the parameter type
		}

		args.push((name, typename));
	}

	Ok(args)
}
