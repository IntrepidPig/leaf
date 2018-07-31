use ast::parser::*;

pub struct IfTaker;

impl ExpressionTaker for IfTaker {
	type Args = ();

	fn take_expression<'a>(&self, in_tokens: &'a [TokenTree], _args: Self::Args) -> ParseResult<'a, Expression> {
		match in_tokens.get(0) {
			Some(TokenTree::Block(BlockType::If, tokens, outer_span, _inner_span)) => {
				let (until_then, leftovers) = operation::split_at(tokens, Box::new(|token| token.is_then()), false);
				let condition = parse_block(until_then)?;

				match leftovers.get(0) {
					Some(&TokenTree::Token(Token {
						kind: TokenKind::Keyword(Keyword::Then),
						..
					})) => {},
					t => {
						return Err(ParseError {
							kind: ParseErrorKind::Expected(vec![Expected::Keyword(Keyword::Then)]),
							// TODO better default location
							span: t.map(|t| t.get_span()).unwrap_or(*outer_span),
						}.into());
					}, // Should never happen?
				}

				let (body_tokens, leftovers) =
					operation::split_at(&leftovers[1..], Box::new(|token| token.is_else()), false);

				// TODO elseif
				let body = parse_block(body_tokens)?;

				let else_block = if leftovers.is_empty() {
					None
				} else {
					match leftovers.get(0) {
						Some(TokenTree::Token(Token {
							kind: TokenKind::Keyword(Keyword::Else),
							..
						})) => {
							let else_body = parse_block(&leftovers[1..])?;

							Some(else_body)
						},
						// needed else or nothing after then
						t => {
							return Err(ParseError {
								kind: ParseErrorKind::Expected(vec![Expected::Keyword(Keyword::Then)]),
								// TODO better default location
								span: t.map(|t| t.get_span()).unwrap_or(*outer_span),
							}.into());
						},
					}
				};

				Ok(Some((
					Expression::If(Box::new(If {
						condition: Expression::Block(Box::new(condition)),
						body: Expression::Block(Box::new(body)),
						elif: None,
						else_block: else_block.map(|b| Expression::Block(Box::new(b))),
					})),
					&in_tokens[1..],
				)))
			},
			_ => Ok(None),
		}
	}
}
