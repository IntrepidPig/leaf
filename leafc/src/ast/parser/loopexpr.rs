use ast::parser::*;

#[derive(Default)]
pub struct LoopTaker;

impl LoopTaker {
	pub fn new() -> Self {
		LoopTaker
	}
}

impl ExpressionTaker for LoopTaker {
	type Args = ();

	fn take_expression<'a>(&self, in_tokens: &'a [TokenTree], _args: Self::Args) -> ParseResult<'a, Expression> {
		match in_tokens.get(0) {
			Some(TokenTree::Block(BlockType::Loop, tokens, start_location, end_location)) => {
				let expr = parse_block(tokens)?;

				if expr.output.is_some() {
					// Loop blocks can't have an output
					return Err(ParseError {
						kind: ParseErrorKind::LoopWithOutput,
						location: *end_location
					}.into());
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
