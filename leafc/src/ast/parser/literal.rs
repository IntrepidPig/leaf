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

	fn take_expression<'a>(&self, in_tokens: &'a [TokenTree], _args: Self::Args) -> ParseResult<'a, Expression> {
		Ok(Some((
			match in_tokens.get(0) {
				Some(TokenTree::Token(Token {
					kind: TokenKind::NumberLiteral(ref num),
					..
				})) => Expression::NumberLiteral(*num),
				Some(TokenTree::Token(Token {
					kind: TokenKind::StringLiteral(ref string),
					..
				})) => Expression::StringLiteral(string.clone()),
				Some(TokenTree::Token(Token {
					kind: TokenKind::Keyword(Keyword::True),
					..
				})) => Expression::BoolLiteral(true),
				Some(TokenTree::Token(Token {
					kind: TokenKind::Keyword(Keyword::False),
					..
				})) => Expression::BoolLiteral(false),
				_ => return Ok(None),
			},
			&in_tokens[1..],
		)))
	}
}
