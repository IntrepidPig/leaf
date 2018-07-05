use ast::parser::*;

/// Gets the next break statement
pub struct FunctionCallTaker;

impl ExpressionTaker for FunctionCallTaker {
	type Args = ();

	fn take_expression<'a>(
		&self,
		in_tokens: &'a [TokenTree],
		_args: Self::Args,
	) -> Result<Option<(Expression, &'a [TokenTree])>, Error<ParseError>> {
		if in_tokens.is_empty() {
			return Ok(None);
		}
		
		let mut tokens = in_tokens;
		
		let name = match tokens.get(0) {
			Some(TokenTree::Token(Token::Name(ref name))) => {
				tokens = &tokens[1..];
				name.clone()
			},
			_ => return Ok(None),
		};

		let args = match tokens.get(0) {
			Some(TokenTree::Paren(ref args_tokens)) => {
				tokens = &tokens[1..];
				parse_args(args_tokens)?
			},
			_ => return Ok(None) // it could be an identifier
		};
		
		Ok(Some((Expression::FunctionCall {
			name,
			args,
		}, tokens)))
	}
}

fn parse_args(in_tokens: &[TokenTree]) -> Result<Vec<Expression>, Error<ParseError>> {
	let mut each_args_tokens = Vec::new();
	in_tokens.split(|token| token.is_comma()).map(|tokens| each_args_tokens.push(tokens)).collect::<()>();
	
	let mut args = Vec::new();
	
	for arg_tokens in each_args_tokens {
		if let Some((expression, leftovers)) = next_expression(arg_tokens, Box::new(|_| false))? {
			if !leftovers.is_empty() {
				return Err(ParseError::Other.into()) // didn't parse the entire argument expression tokens
			}
			
			args.push(expression);
		}
	}
	
	Ok(args)
}