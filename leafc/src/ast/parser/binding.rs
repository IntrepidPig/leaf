use ast::parser::*;

/// Gets the next let binding
pub struct BindingTaker;

impl ExpressionTaker for BindingTaker {
	type Args = ();

	fn take_expression<'a>(&self, in_tokens: &'a [TokenTree], _args: Self::Args) -> ParseResult<'a, Expression> {
		if in_tokens.is_empty() {
			return Ok(None);
		}

		let mut tokens = in_tokens;

		match tokens.get(0) {
			Some(TokenTree::Token(Token::Keyword(Keyword::Let))) => {
				tokens = &tokens[1..];
			},
			_ => return Ok(None),
		}

		let mutable = match tokens.get(0) {
			Some(TokenTree::Token(Token::Keyword(Keyword::Mutable))) => {
				tokens = &tokens[1..];
				true
			},
			_ => false,
		};

		let ident = match tokens.get(0) {
			Some(TokenTree::Token(Token::Name(ref name))) => Identifier::try_from_str(name),
			_ => return Err(ParseError::Expected(vec![Expected::Identifier]).into()),
		};
		tokens = &tokens[1..];

		let bindtype = match tokens.get(0) {
			Some(TokenTree::Token(Token::Symbol(TokenSymbol::Colon))) => {
				tokens = &tokens[1..];
				let (typename, leftovers) = if let Some(res) = next_type(tokens)? {
					res
				} else {
					return Err(ParseError::Expected(vec![Expected::Typename]).into());
				};
				tokens = leftovers;
				Some(typename)
			},
			_ => None,
		};

		match tokens.get(0) {
			Some(TokenTree::Token(Token::Symbol(TokenSymbol::Assign))) => {
				tokens = &tokens[1..];
			},
			// expected assign symbol in let binding
			_ => return Err(ParseError::Expected(vec![Expected::Symbol(TokenSymbol::Assign)]).into()),
		}

		let (bindexpr, leftovers) = if let Some(res) = next_expression(tokens, Box::new(|token| token.is_semicolon()))?
		{
			res
		} else {
			// expected an expression after the assign symbol
			return Err(ParseError::Expected(vec![Expected::Expression]).into());
		};

		tokens = leftovers;

		Ok(Some((
			Expression::Binding(Box::new(Binding {
				mutable,
				ident,
				bind_type: bindtype,
				val: Some(bindexpr),
			})),
			tokens,
		)))
	}
}
