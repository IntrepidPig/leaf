use ast::parser::*;

pub struct IfTaker;

impl ExpressionTaker for IfTaker {
	type Args = ();

	fn take_expression<'a>(&self, in_tokens: &'a [TokenTree], _args: Self::Args) -> Result<Option<(Expression, &'a [TokenTree])>, Error<ParseError>> {
		match in_tokens.get(0) {
			Some(TokenTree::Token(Token::Keyword(Keyword::If))) => {
				let (condition, sub_leftovers) = if let Some((condition, extra_leftovers)) =
					next_expression(&in_tokens[1..], Box::new(|token| token.is_then()))?
				{
					match extra_leftovers.get(0) {
						Some(TokenTree::Token(Token::Keyword(Keyword::Then))) => {},
						_ => return Err(ParseError::Other.into()), // Missing a then keyword in the if
					}

					(condition, extra_leftovers)
				} else {
					return Err(ParseError::Other.into()); // The if statement had no condition
				};

				let (body, sub_leftovers) = if let Some(token) = sub_leftovers.get(0) {
					match token {
						TokenTree::Token(Token::Keyword(Keyword::Then)) => {
							if let Some((body, extra_leftovers)) = next_expression(
								&sub_leftovers[1..],
								Box::new(|token| token.is_else() || token.is_semicolon()),
							)? {
								(body, extra_leftovers)
							} else {
								// There was no condition block after the is statement
								return Err(ParseError::Other.into());
							}
						},
						// There was no then keyword after the condition
						_ => return Err(ParseError::Other.into()),
					}
				} else {
					// There was no then keyword after the condition
					return Err(ParseError::Other.into());
				};

				let (else_block, sub_leftovers) = if let Some(token) = sub_leftovers.get(0) {
					match token {
						TokenTree::Token(Token::Symbol(TokenSymbol::Semicolon)) => {
							(None, sub_leftovers)
						},
						TokenTree::Token(Token::Keyword(Keyword::Else)) => {
							if let Some((body, extra_leftovers)) = next_expression(
								&sub_leftovers[1..],
								Box::new(|token| token.is_semicolon()),
							)? {
								(Some(body), extra_leftovers)
							} else {
								// There should be an expression after the else keyword
								// and before the semicolon
								return Err(ParseError::Other.into());
							}
						},
						// There should be an expression after the else keyword and before the semicolon
						_ => return Err(ParseError::Other.into()),
					}
				} else {
					(None, sub_leftovers)
				};

				// TODO support elif

				Ok(Some((
					Expression::If(Box::new(If {
						condition,
						body,
						elif: None,
						else_block,
					})),
					sub_leftovers,
				)))
			},
			_ => return Ok(None)
		}
	}
}

