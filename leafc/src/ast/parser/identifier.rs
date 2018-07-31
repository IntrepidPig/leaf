use ast::parser::*;

#[derive(Default)]
pub struct IdentifierTaker;

impl IdentifierTaker {
	pub fn new() -> Self {
		IdentifierTaker
	}
}

impl ExpressionTaker for IdentifierTaker {
	type Args = ();

	fn take_expression<'a>(&self, in_tokens: &'a [TokenTree], _args: Self::Args) -> ParseResult<'a, Expression> {
		Ok(Some((
			match in_tokens.get(0) {
				Some(TokenTree::Token(Token {
					kind: TokenKind::Name(ref name),
					..
				})) => Expression::Identifier(Identifier::from_string(name.clone())),
				_ => return Ok(None),
			},
			&in_tokens[1..],
		)))
	}
}
