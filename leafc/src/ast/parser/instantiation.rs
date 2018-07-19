use ast::parser::*;

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
	) -> Result<Option<(Expression, &'a [TokenTree])>, Error<ParseError>> {
		let mut tokens = in_tokens;
		
		let name = match tokens.get(0) {
			Some(TokenTree::Token(Token::Name(ref name))) => {
				tokens = &tokens[1..];
				name.clone()
			},
			_ => return Ok(None),
		};
		
		let mut values = Vec::new();
		
		match tokens.get(0) {
			Some(TokenTree::Brace(ref tokens)) => {
				for expression_tokens in separated::parse_separated(tokens, |token| token.is_comma())? {
					let (expr, leftovers) = if let Some(expr) = next_expression(expression_tokens, Box::new(|_| false))? {
						expr
					} else {
						return Err(ParseError::Other.into()) // Expected an expression in the instantiation
					};
					
					values.push(expr);
				}
			},
			_ => return Ok(None), // debug
		}
		
		tokens = &tokens[1..];
		
		Ok(Some((Expression::Instantiation(name, values), tokens)))
	}
}
