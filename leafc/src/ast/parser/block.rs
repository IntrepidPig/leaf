use ast::parser::*;

/// Gets the next block from a token list, starting from an open brace ('{') and parsing everything
/// until the close brace, and returning everything after the last close brace as leftovers
pub struct BlockTaker;

impl ExpressionTaker for BlockTaker {
	type Args = ();

	fn take_expression<'a>(
		&self,
		in_tokens: &'a [TokenTree],
		_args: Self::Args
	) -> Result<Option<(Expression, &'a [TokenTree])>, Error<ParseError>> {
		debug!("Parsing block from: {:?}", in_tokens);
		if in_tokens.is_empty() {
			return Ok(None);
		}
		match in_tokens[0] {
			TokenTree::Brace(ref tokens) => {
				if let Some(ast) = next_syntaxtree(tokens)? {
					return Ok(Some((Expression::Block(Box::new(ast)), &tokens[1..])));
				} else {
					return Err(ParseError::UnexpectedToken.into());
				}
			},
			_ => return Ok(None),
		}
	}
}
