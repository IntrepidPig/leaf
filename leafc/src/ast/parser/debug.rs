use ast::parser::*;

/// Gets the next debug statement
// TODO include expression original string repr with debug
pub struct DebugTaker;

impl ExpressionTaker for DebugTaker {
	type Args = ();

	fn take_expression<'a>(&self, in_tokens: &'a [TokenTree], _args: Self::Args) -> ParseResult<'a, Expression> {
		if in_tokens.is_empty() {
			return Ok(None);
		}

		let mut tokens = in_tokens;

		match tokens[0] {
			// If the first token is debug it's a debug statement
			TokenTree::Token(Token::Keyword(Keyword::Debug)) => {
				// Get the expression after the keyword
				let expr = if let Some((expr, leftovers)) =
					next_expression(&tokens[1..], Box::new(|token| token.is_semicolon()))?
				{
					tokens = leftovers;
					expr
				} else {
					return Ok(None);
				};

				// Return the expression being debugged
				Ok(Some((Expression::Debug(Box::new(expr)), tokens)))
			},
			_ => Ok(None),
		}
	}
}
