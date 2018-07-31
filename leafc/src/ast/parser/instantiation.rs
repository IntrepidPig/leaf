use ast::parser::*;

#[derive(Default)]
pub struct InstantiationTaker;

impl InstantiationTaker {
	pub fn new() -> Self {
		InstantiationTaker
	}
}

impl ExpressionTaker for InstantiationTaker {
	type Args = ();

	fn take_expression<'a>(&self, in_tokens: &'a [TokenTree], _args: Self::Args) -> ParseResult<'a, Expression> {
		let mut tokens = in_tokens;

		let (typename, leftovers) = if let Some(res) = pathitem::next_type(tokens)? {
			res
		} else {
			return Ok(None);
		};

		tokens = leftovers;

		let mut values: Vec<(Identifier, Expression)> = Vec::new();

		match tokens.get(0) {
			Some(TokenTree::Block(BlockType::Brace, ref tokens, start_location, _end_location)) => {
				let mut last_location = *start_location;
				for field_tokens in separated::parse_separated(tokens, |token| token.is_comma())? {
					let name = match field_tokens.get(0) {
						Some(TokenTree::Token(Token {
							kind: TokenKind::Name(ref ident),
							location,
						})) => {
							last_location = *location;
							ident.clone()
						},
						// instantiation needed field name
						t => {
							return Err(ParseError {
								kind: ParseErrorKind::Expected(vec![Expected::Identifier]),
								location: t.map(|t| t.get_location()).unwrap_or(last_location),
							}.into())
						},
					};

					match field_tokens.get(1) {
						Some(TokenTree::Token(Token {
							kind: TokenKind::Symbol(TokenSymbol::Colon),
							location,
						})) => {
							last_location = *location;
						},
						// Needed colon after field name
						t => {
							return Err(ParseError {
								kind: ParseErrorKind::Expected(vec![Expected::Symbol(TokenSymbol::Colon)]),
								location: t.map(|t| t.get_location()).unwrap_or(last_location),
							}.into())
						},
					}

					let (expr, _leftovers) =
						if let Some(expr) = next_expression(&field_tokens[2..], Box::new(|_| false))? {
							// TODO remove panic
							expr
						} else {
							// Expected an expression in the instantiation
							return Err(ParseError {
								kind: ParseErrorKind::Expected(vec![Expected::Expression]),
								location: last_location,
							}.into());
						};

					values.push((Identifier::from_string(name), expr));
				}
			},
			_ => return Ok(None), // debug
		}

		tokens = &tokens[1..];

		Ok(Some((Expression::Instantiation(typename, values), tokens)))
	}
}
