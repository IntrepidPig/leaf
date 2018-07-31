use ast::parser::*;

/// Gets the next break statement
pub struct FunctionCallTaker;

impl ExpressionTaker for FunctionCallTaker {
	type Args = ();

	fn take_expression<'a>(&self, in_tokens: &'a [TokenTree], _args: Self::Args) -> ParseResult<'a, Expression> {
		if in_tokens.is_empty() {
			return Ok(None);
		}

		let mut tokens = in_tokens;

		let (path, leftovers) = if let Some(res) = pathitem::next_path(tokens)? {
			res
		} else {
			return Ok(None);
		};

		tokens = leftovers;

		let name = match tokens.get(0) {
			Some(TokenTree::Token(Token {
				kind: TokenKind::Name(ref name),
				..
			})) => {
				tokens = &tokens[1..];
				Identifier::from_string(name.clone())
			},
			_ => return Ok(None),
		};

		let name = PathItem {
			module_path: path,
			item: name,
		};

		let args = match tokens.get(0) {
			Some(TokenTree::Block(BlockType::Paren, ref args_tokens, _, _)) => {
				tokens = &tokens[1..];
				parse_args(args_tokens)?
			},
			_ => return Ok(None), // it could be an identifier
		};

		Ok(Some((Expression::FunctionCall { name, args }, tokens)))
	}
}

fn parse_args(in_tokens: &[TokenTree]) -> Result<Vec<Expression>, Error<ParseError>> {
	let mut each_args_tokens = Vec::new();
	for tokens in in_tokens.split(|token| token.is_comma()) {
		each_args_tokens.push(tokens)
	}

	let mut args = Vec::new();

	for arg_tokens in each_args_tokens {
		if let Some((expression, leftovers)) = next_expression(arg_tokens, Box::new(|_| false))? {
			if !leftovers.is_empty() {
				// didn't parse the entire argument expression tokens
				return Err(ParseError {
					kind: ParseErrorKind::UnexpectedToken,
					span: leftovers[0].get_span(),
				}.into());
			}

			args.push(expression);
		}
	}

	Ok(args)
}
