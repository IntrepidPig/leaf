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

	fn take_expression<'a>(
		&self,
		in_tokens: &'a [TokenTree],
		_args: Self::Args,
	) -> ParseResult<'a, Expression> {
		let mut tokens = in_tokens;

		let (typename, leftovers) = if let Some(res) = pathitem::next_type(tokens)? {
			res
		} else {
			return Ok(None);
		};

		tokens = leftovers;

		let mut values: Vec<(Identifier, Expression)> = Vec::new();

		match tokens.get(0) {
			Some(TokenTree::Brace(ref tokens)) => {
				for field_tokens in separated::parse_separated(tokens, |token| token.is_comma())? {
					let name = match field_tokens.get(0) {
						Some(TokenTree::Token(Token::Name(ref ident))) => ident.clone(),
						// instantiation needed field name
						_ => return Err(ParseError::Other.into()),
					};

					match field_tokens.get(1) {
						Some(TokenTree::Token(Token::Symbol(TokenSymbol::Colon))) => {},
						// Needed colon after field name
						_ => return Err(ParseError::Other.into()),
					}

					let (expr, _leftovers) = if let Some(expr) =
						next_expression(&field_tokens[2..], Box::new(|_| false))?
					{
						expr
					} else {
						// Expected an expression in the instantiation
						return Err(ParseError::Other.into());
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
