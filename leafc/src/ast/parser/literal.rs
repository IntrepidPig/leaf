use ast::parser::*;

pub struct LiteralTaker;

impl LiteralTaker {
	pub fn new() -> Self {
		LiteralTaker
	}
}

impl ExpressionTaker for LiteralTaker {
	type Args = ();

	fn take_expression<'a>(
		&self,
		in_tokens: &'a [TokenTree],
		_args: Self::Args,
	) -> Result<Option<(Expression, &'a [TokenTree])>, Error<ParseError>> {
		Ok(Some((
			match in_tokens.get(0) {
				Some(TokenTree::Token(Token::NumberLiteral(ref num))) => {
					Expression::NumberLiteral(num.clone())
				},
				Some(TokenTree::Token(Token::StringLiteral(ref string))) => {
					Expression::StringLiteral(string.clone())
				},
				Some(TokenTree::Token(Token::Keyword(Keyword::True))) => {
					Expression::BoolLiteral(true)
				},
				Some(TokenTree::Token(Token::Keyword(Keyword::False))) => {
					Expression::BoolLiteral(false)
				},
				_ => return Ok(None),
			},
			&in_tokens[1..],
		)))
	}
}
