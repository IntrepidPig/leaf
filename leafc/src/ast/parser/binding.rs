use ast::parser::*;

/// Gets the next let binding
pub struct BindingTaker;

impl ExpressionTaker for BindingTaker {
	type Args = ();

	fn take_expression<'a>(
		&self,
		in_tokens: &'a [TokenTree],
		_args: Self::Args,
	) -> Result<Option<(Expression, &'a [TokenTree])>, Error<ParseError>> {
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
			Some(TokenTree::Token(Token::Name(ref name))) => Identifier::from_str(name),
			_ => return Err(ParseError::Other.into()), // expected an identifier after let
		};
		tokens = &tokens[1..];

		let bindtype = match tokens.get(0) {
			Some(TokenTree::Token(Token::Symbol(TokenSymbol::Colon))) => {
				tokens = &tokens[1..];
				let (typename, leftovers) = if let Some(res) = next_type(tokens)? {
					res
				} else {
					return Err(ParseError::Other.into()); // expected a type after let binding colon
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
			_ => return Err(ParseError::Other.into()), // expected assign symbol in let binding
		}

		let (bindexpr, leftovers) =
			if let Some(res) = next_expression(tokens, Box::new(|token| token.is_semicolon()))? {
				res
			} else {
				// expected an expression after the assign symbol
				return Err(ParseError::Other.into());
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
