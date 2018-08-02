use ast::parser::*;

/// Gets the next break statement
pub struct BreakTaker;

impl ExpressionTaker for BreakTaker {
	type Args = ();

	fn next_expression(&self, stream: &mut TokenStream, _args: Self::Args) -> ParseResult<Expression> {
		if let TokenTree::Token(Token { kind: TokenKind::Keyword(Keyword::Break), .. }) = stream.take_tokentree()? { } else {
			return Ok(None)
		}
		
		let expr = if !stream.is_empty() {
			Some(Box::new(operation::parse_expression(stream)?))
		} else {
			None
		};
		
		Ok(Some(Expression::Break(expr)))
	}
}
