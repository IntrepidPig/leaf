use ast::parser::*;

/// Gets the next break statement
pub struct BreakTaker;

impl ExpressionTaker for BreakTaker {
	type Args = ();

	fn take_expression<'a>(
		&self,
		in_tokens: &'a [TokenTree],
		_args: Self::Args
	) -> Result<Option<(Expression, &'a [TokenTree])>, Error<ParseError>> {
		debug!("Parsing break from: {:?}", in_tokens);
		if in_tokens.is_empty() {
			return Ok(None);
		}

		match in_tokens[0] {
			// If the first token is debug it's a break statement
			TokenTree::Token(Token::Keyword(Keyword::Break)) => {
				// Get the expression after the keyword
				if let Some((expr, leftovers)) =
					next_expression(&in_tokens[1..], Box::new(|token| token.is_semicolon()))?
				{
					Ok(Some((Expression::Break(Some(Box::new(expr))), leftovers)))
				} else {
					Ok(Some((Expression::Break(None), &in_tokens[1..])))
				}
			},
			_ => Ok(None),
		}
	}
}