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

		match tokens[0] {
			// If the first token is `let` then it's a let binding
			TokenTree::Token(Token::Keyword(Keyword::Let)) => {
				// Get the identifier (which should always be the second token)
				let ident = if let TokenTree::Token(Token::Name(ref name)) = tokens[1] {
					name.to_owned()
				} else {
					return Err(ParseError::Other.into()); // expected an identifier/pattern
				};

				// If there's a type annotation, parse it
				// Set offset to how many tokens the type annotation occupied
				let mut offset = 0;
				let mut type_annotation: Option<String> =
					if let TokenTree::Token(Token::Symbol(TokenSymbol::Colon)) = tokens[2] {
						// if there's a type annotation
						// change token offset
						unimplemented!()
					} else {
						None
					};

				// Make sure there's an assign symbol after the identifier or type annotation
				if let TokenTree::Token(Token::Symbol(TokenSymbol::Assign)) = tokens[2 + offset] {
				} else {
					return Err(ParseError::Other.into()); // There should have been an assign symbol
				}

				// Parse the expression part of the binding
				let expr = if let Some((expr, leftovers)) = next_expression(
					&tokens[2 + offset + 1..],
					Box::new(|token| token.is_semicolon()),
				)? {
					tokens = leftovers;
					expr
				} else {
					return Err(ParseError::Other.into()); // Failed to parse the expression in the let binding
				};

				Ok(Some((
					Expression::Binding(Box::new(Binding {
						mutable: false,
						ident,
						bind_type: type_annotation,
						val: Some(expr),
					})),
					tokens,
				)))
			},
			_ => Ok(None),
		}
	}
}
