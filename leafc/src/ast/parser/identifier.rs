use ast::parser::*;

pub struct IdentifierTaker;

impl IdentifierTaker {
	pub fn new() -> Self {
		IdentifierTaker
	}
}

impl ExpressionTaker for IdentifierTaker {
	type Args = ();

	fn take_expression<'a>(
		&self,
		in_tokens: &'a [TokenTree],
		_args: Self::Args,
	) -> Result<Option<(Expression, &'a [TokenTree])>, Error<ParseError>> {
		Ok(Some((match in_tokens.get(0) {
			Some(TokenTree::Token(Token::Name(ref name))) => {
				Expression::Identifier(Identifier::from_string(name.clone()))
			},
			_ => return Ok(None)
		}, &in_tokens[1..])))
	}
}