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

	fn next_expression(&self, stream: &mut TokenStream, _args: Self::Args) -> ParseResult<Expression> {
		if let TokenTree::Token(Token {
			kind: TokenKind::Keyword(Keyword::Loop),
			..
		}) = stream.take_tokentree()?
		{
		} else {
			return Ok(None);
		}

		let body = if let TokenTree::Block(Bracket::Curly, ref mut loop_token_stream, _, _) = stream.take_tokentree()? {
			parse_block(loop_token_stream)?
		} else {
			return Err(ParseError::expected(vec![Expected::Block], &*stream).into());
		};

		Ok(Some(Expression::Loop(Box::new(Expression::Block(
			Box::new(body),
		)))))
	}
}
