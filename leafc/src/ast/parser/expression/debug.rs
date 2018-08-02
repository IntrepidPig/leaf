use ast::parser::*;

/// Gets the next debug statement
// TODO include expression original string repr with debug
pub struct DebugTaker;

impl ExpressionTaker for DebugTaker {
	type Args = ();

	fn next_expression(&self, stream: &mut TokenStream, _args: Self::Args) -> ParseResult<Expression> {
		if let TokenTree::Token(Token {
			kind: TokenKind::Keyword(Keyword::Debug),
			..
		}) = stream.take_tokentree()?
		{
		} else {
			return Ok(None);
		};

		let expr = operation::parse_expression(stream)?;

		Ok(Some(Expression::Debug(Box::new(expr))))
	}
}
