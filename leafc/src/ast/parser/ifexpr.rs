use ast::parser::*;

pub struct IfTaker;

impl ExpressionTaker for IfTaker {
	type Args = ();

	fn take_expression<'a>(
		&self,
		in_tokens: &'a [TokenTree],
		_args: Self::Args,
	) -> Result<Option<(Expression, &'a [TokenTree])>, Error<ParseError>> {
		match in_tokens.get(0) {
			Some(TokenTree::If(tokens)) => {
				let (condition, leftovers) = if let Some(res) = next_expression(tokens, Box::new(|token| token.is_then()))? {
					res
				} else {
					return Err(ParseError::Other.into()) // Failed to parse the if condition expression
				};
								
				if !(leftovers.get(0) == Some(&TokenTree::Token(Token::Keyword(Keyword::Then)))) {
					return Err(ParseError::Other.into()) // needed `then` keyword
				}
				
				// TODO elseif
				
				let (body, leftovers) = if let Some(res) = next_expression(&leftovers[1..], Box::new(|token| token.is_else() || token.is_end()))? {
					res
				} else {
					return Err(ParseError::Other.into()) // Failed to parse the if condition expression
				};
								
				let (else_block, leftovers) = if leftovers.is_empty() {
					(None, leftovers)
				} else {
					match leftovers.get(0) {
						Some(TokenTree::Token(Token::Keyword(Keyword::Else))) => {
							let (else_body, leftovers) = if let Some(res) = next_expression(&leftovers[1..], Box::new(|_| false))? {
								res
							} else {
								return Err(ParseError::Other.into()) // Failed to parse the if condition expression
							};
							
							(Some(else_body), leftovers)
						},
						_ => return Err(ParseError::Other.into()) // needed else or nothing after then
					}
				};
								
				if !leftovers.is_empty() {
					return Err(ParseError::Other.into()) // Failed to parse all tokens in if
				}
				
				Ok(Some((
					Expression::If(Box::new(If {
						condition,
						body,
						elif: None,
						else_block,
					})),
					&in_tokens[1..],
				)))
			},
			_ => return Ok(None),
		}
	}
}
