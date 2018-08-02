use ast::parser::*;

#[derive(Default)]
pub struct LiteralTaker;

impl LiteralTaker {
	pub fn new() -> Self {
		LiteralTaker
	}
}

impl ExpressionTaker for LiteralTaker {
	type Args = ();

	fn next_expression(&self, stream: &mut TokenStream, _args: Self::Args) -> ParseResult<Expression> {
		Ok(Some(match stream.take_tokentree()? {
			TokenTree::Token(Token { kind: TokenKind::NumberLiteral(ref val), .. }) => Expression::NumberLiteral(*val),
			TokenTree::Token(Token { kind: TokenKind::StringLiteral(ref val), .. }) => Expression::StringLiteral(val.clone()),
			TokenTree::Token(Token { kind: TokenKind::Keyword(Keyword::True), .. }) => Expression::BoolLiteral(true),
			TokenTree::Token(Token { kind: TokenKind::Keyword(Keyword::False), .. }) => Expression::BoolLiteral(false),
			_ => return Ok(None),
		}))
	}
}
