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
		
		let (typename, leftovers) = if let Some(res) = pathitem::next_type(tokens)? {
			res
		} else {
			return Ok(None)
		};
		
		tokens = leftovers;
		
		let mut values: Vec<(Identifier, Expression)> = Vec::new();
		
		match tokens.get(0) {
			Some(TokenTree::Brace(ref tokens)) => {
				for field_tokens in separated::parse_separated(tokens, |token| token.is_comma())? {
					let name = match field_tokens.get(0) {
						Some(TokenTree::Token(Token::Name(ref ident))) => {
							ident.clone()
						},
						_ => return Err(ParseError::Other.into()) // instantiation needed field name
					};
					
					match field_tokens.get(1) {
						Some(TokenTree::Token(Token::Symbol(TokenSymbol::Colon))) => {},
						_ => return Err(ParseError::Other.into()) // Needed colon after field name
					}
					
					let (expr, _leftovers) = if let Some(expr) = next_expression(&field_tokens[2..], Box::new(|_| false))? {
						expr
					} else {
						return Err(ParseError::Other.into()) // Expected an expression in the instantiation
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
