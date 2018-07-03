use ast::parser::*;

pub struct LoopTaker;

impl ExpressionTaker for LoopTaker {
	type Args = ();

	fn take_expression<'a>(
		&self,
		in_tokens: &'a [TokenTree],
		_args: Self::Args,
	) -> Result<Option<(Expression, &'a [TokenTree])>, Error<ParseError>> {
		match in_tokens.get(0) {
			Some(TokenTree::Token(Token::Keyword(Keyword::Loop))) => {
				// If theres a brace block after loop keyword, include only
				// that in the loop expression
				if let Some(TokenTree::Brace(_)) = in_tokens.get(1) {
					if let Some((block, extra_leftovers)) =
						next_expression(&in_tokens[1..], Box::new(|token| !token.is_brace_expr()))?
					{
						Ok(Some((Expression::Loop(Box::new(block)), extra_leftovers)))
					} else {
						// Failed to parse the block after the loop
						Err(ParseError::Other.into())
					}
				} else {
					if let Some((expr, extra_leftovers)) =
						next_expression(&in_tokens[1..], Box::new(|token| token.is_semicolon()))?
					{
						Ok(Some((
							Expression::Loop(Box::new(Expression::Block(Box::new(Block {
								block: vec![expr],
								output: None,
							})))),
							extra_leftovers,
						)))
					} else {
						// Failed to parse the expression after the loop
						Err(ParseError::Other.into())
					}
				}
			},
			_ => Ok(None),
		}
	}
}
