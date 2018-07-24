use ast::parser::*;

pub struct LoopTaker;

impl LoopTaker {
	pub fn new() -> Self {
		LoopTaker
	}
}

impl ExpressionTaker for LoopTaker {
	type Args = ();

	fn take_expression<'a>(
		&self,
		in_tokens: &'a [TokenTree],
		_args: Self::Args,
	) -> Result<Option<(Expression, &'a [TokenTree])>, Error<ParseError>> {
		match in_tokens.get(0) {
			Some(TokenTree::Loop(tokens)) => {
				let expr = parse_block(tokens)?;

				if expr.output.is_some() {
					return Err(ParseError::Other.into()); // Loop blocks can't have an output
				}

				Ok(Some((
					Expression::Loop(Box::new(Expression::Block(Box::new(expr)))),
					&in_tokens[1..],
				)))
			},
			_ => Ok(None),
		}
	}
}
