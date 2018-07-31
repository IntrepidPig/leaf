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

		let last_location = match tokens.get(0) {
			Some(TokenTree::Token(Token {
				kind: TokenKind::Keyword(Keyword::Let),
				location,
			})) => {
				tokens = &tokens[1..];
				*location
			},
			_ => return Ok(None),
		};

		let (mutable, last_location) = match tokens.get(0) {
			Some(TokenTree::Token(Token {
				kind: TokenKind::Keyword(Keyword::Mutable),
				location,
			})) => {
				tokens = &tokens[1..];
				(true, *location)
			},
			_ => (false, last_location),
		};

		let (ident, last_location) = match tokens.get(0) {
			Some(TokenTree::Token(Token {
				kind: TokenKind::Name(ref name),
				location,
			})) => (Identifier::try_from_str(name), *location),
			Some(t) => {
				return Err(ParseError {
					kind: ParseErrorKind::Expected(vec![Expected::Identifier]),
					location: t.get_location(),
				}.into())
			},
			None => {
				return Err(ParseError {
					kind: ParseErrorKind::Expected(vec![Expected::Identifier]),
					location: last_location,
				}.into())
			},
		};
		tokens = &tokens[1..];

		let (bindtype, last_location) = match tokens.get(0) {
			Some(TokenTree::Token(Token {
				kind: TokenKind::Symbol(TokenSymbol::Colon),
				location,
			})) => {
				tokens = &tokens[1..];
				let (typename, leftovers) = if let Some(res) = next_type(tokens)? {
					res
				} else {
					return Err(ParseError {
						kind: ParseErrorKind::Expected(vec![Expected::Typename]),
						location: *location,
					}.into());
				};
				tokens = leftovers;
				(Some(typename), *location)
			},
			_ => (None, last_location),
		};

		let last_location = match tokens.get(0) {
			Some(TokenTree::Token(Token {
				kind: TokenKind::Symbol(TokenSymbol::Assign),
				location,
			})) => {
				tokens = &tokens[1..];
				*location
			},
			// expected assign symbol in let binding
			t => {
				return Err(ParseError {
					kind: ParseErrorKind::Expected(vec![Expected::Symbol(TokenSymbol::Assign)]),
					location: t.map(|t| t.get_location()).unwrap_or(last_location),
				}.into())
			},
		};

		let (bindexpr, leftovers) = if let Some(res) = next_expression(tokens, Box::new(|token| token.is_semicolon()))?
		{
			res
		} else {
			// expected an expression after the assign symbol
			return Err(ParseError {
				kind: ParseErrorKind::Expected(vec![Expected::Expression]),
				location: last_location,
			}.into());
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
